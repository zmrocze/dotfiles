open Ir
open Ir_utils
open Analysis

module type DOMAIN = sig
  type domain

  val string_of_domain : domain -> string
end

module type DOMAIN_AND_BLOCK_ANALYSIS = sig
  include DOMAIN

  val analyse_block :
    ControlFlowGraph.t ->
    domain Knowledge.table ->
    label ->
    domain BlockKnowledge.t
end

(*
module TableVisualizer = struct

  let stringize_table (to_string: 'a -> string) result = 
    let new_result = Hashtbl.create 513 in
    let f k v =
      let kw = Knowledge.make (to_string @@ Knowledge.pre v) (to_string @@ Knowledge.post v) in
      Hashtbl.replace new_result k kw
    in
    Hashtbl.iter f result;
    new_result

  let stringize_full_table (to_string: 'a -> string) result =
    let visualize_kw v = Knowledge.make (to_string @@ Knowledge.pre v) (to_string @@ Knowledge.post v) in
    let visualize_instr (kw, instr) = (visualize_kw kw, instr) in
    let visualize_body = List.map visualize_instr in 
    let visualize_terminator (kw, terminator) = (visualize_kw kw, terminator) in
    let new_result = Hashtbl.create 513 in
    let f k v =
      let pre = to_string @@ BlockKnowledge.pre v in
      let post = to_string @@ BlockKnowledge.post v in
      let body = visualize_body @@ BlockKnowledge.body v in
      let terminator = visualize_terminator @@ BlockKnowledge.terminator v in
      Hashtbl.replace new_result k @@ BlockKnowledge.make ~pre ~post ~body ~terminator
    in
    Hashtbl.iter f result;
    new_result


end
*)

module NgMakeGraphvizVisualizer (D : DOMAIN) = struct
  let visualise_instr (pre, post, instr) =
    let instr = string_of_instr instr in
    String.concat "\n"
      [
        Format.sprintf
          "<tr><td>%s</td><td align='left'><b>%s</b></td><td>%s</td></tr>" pre
          instr post;
      ]

  let visualise_terminator (pre, post, t) =
    let t = string_of_terminator t in
    String.concat "\n"
      [
        Format.sprintf
          "<tr><td>%s</td><td bgcolor='green' ><b>%s</b></td><td>%s</td></tr>"
          pre t post;
      ]

  let block_template_pre pre name =
    [
      Format.sprintf
        "<table cellspacing='0' cellborder='1' align='left' border='0'>";
      Format.sprintf
        "<tr><td colspan='3' port='e' bgcolor='yellow'><b>%s</b></td></tr>" name;
      Format.sprintf "<tr><td colspan='3'>%s</td></tr>" @@ pre;
    ]

  let block_template_post post =
    [
      Format.sprintf "<tr><td colspan='3' port='x'>%s</td></tr>" post;
      Format.sprintf "</table>";
    ]

  let block_template name pre post body =
    String.concat ""
    @@ List.flatten
         [ block_template_pre pre name; body; block_template_post post ]

  let stringize_body body =
    let f (kw, instr) =
      let pre = D.string_of_domain @@ Knowledge.pre kw in
      let post = D.string_of_domain @@ Knowledge.post kw in
      (pre, post, instr)
    in
    List.map f body

  let artificial_body body =
    let f instr = ("", "", instr) in
    List.map f body

  let stringize_terminator (kw, terminator) =
    let pre = D.string_of_domain @@ Knowledge.pre kw in
    let post = D.string_of_domain @@ Knowledge.post kw in
    (pre, post, terminator)

  let artificial_terminator terminator = ("", "", terminator)

  let prepare_block bb_kw body terminator =
    if BlockKnowledge.is_complex bb_kw then
      let sbody = stringize_body @@ BlockKnowledge.body bb_kw in
      let sterm = stringize_terminator @@ BlockKnowledge.terminator bb_kw in
      (sbody, sterm)
    else
      let sbody = artificial_body body in
      let sterm = artificial_terminator terminator in
      (sbody, sterm)

  let compute_block_label cfg table v =
    let v_str = string_of_label v in
    let kw = Hashtbl.find table v in
    let pre = D.string_of_domain @@ BlockKnowledge.pre kw in
    let post = D.string_of_domain @@ BlockKnowledge.post kw in
    if v = ControlFlowGraph.entry_label cfg then
      block_template (Format.sprintf "ENTRY %s" v_str) pre post []
    else if v = ControlFlowGraph.exit_label cfg then
      block_template (Format.sprintf "EXIT %s" v_str) pre post []
    else
      let body = ControlFlowGraph.block cfg v in
      let terminator = ControlFlowGraph.terminator cfg v in
      let sbody, sterm = prepare_block kw body terminator in
      let body =
        List.flatten
          [ List.map visualise_instr sbody; [ visualise_terminator sterm ] ]
      in
      block_template (Format.sprintf "BLOCK %s" v_str) pre post body

  let describe_vertex cfg table v =
    Format.sprintf "N%s[shape=none, margin=0, label=<%s>];" (string_of_label v)
      (compute_block_label cfg table v)

  let describe_outedges cfg v =
    let describe_edge w =
      Format.sprintf "N%s:x -> N%s:e;" (string_of_label v) (string_of_label w)
    in
    String.concat "\n" @@ List.map describe_edge
    @@ ControlFlowGraph.successors cfg v

  let visualize cfg table =
    let labels = ControlFlowGraph.labels cfg in
    let vertices =
      String.concat "\n" @@ List.map (describe_vertex cfg table) labels
    in
    let edges = String.concat "\n" @@ List.map (describe_outedges cfg) labels in
    String.concat "\n"
      [
        "digraph CFG {";
        "node [shape=none; fontname=\"Courier\" fontsize=\"9\"];";
        "ordering=out;";
        vertices;
        edges;
        "}";
      ]
end

module MakeGraphvizVisualizer (D : DOMAIN_AND_BLOCK_ANALYSIS) = struct
  let visualise_instr (kw, instr) =
    let pre = D.string_of_domain @@ Knowledge.pre kw in
    let post = D.string_of_domain @@ Knowledge.post kw in
    let instr = string_of_instr instr in
    String.concat "\n"
      [
        Format.sprintf
          "<tr><td>%s</td><td align='left'><b>%s</b></td><td>%s</td></tr>" pre
          instr post;
      ]

  let visualise_terminator (kw, t) =
    let pre = D.string_of_domain @@ Knowledge.pre kw in
    let post = D.string_of_domain @@ Knowledge.post kw in
    let t = string_of_terminator t in
    String.concat "\n"
      [
        Format.sprintf
          "<tr><td>%s</td><td bgcolor='green' ><b>%s</b></td><td>%s</td></tr>"
          pre t post;
      ]

  let block_template_pre pre name =
    [
      Format.sprintf
        "<table cellspacing='0' cellborder='1' align='left' border='0'>";
      Format.sprintf
        "<tr><td colspan='3' port='e' bgcolor='yellow'><b>%s</b></td></tr>" name;
      Format.sprintf "<tr><td colspan='3'>%s</td></tr>" @@ pre;
    ]

  let block_template_post post =
    [
      Format.sprintf "<tr><td colspan='3' port='x'>%s</td></tr>" post;
      Format.sprintf "</table>";
    ]

  let block_template name pre post body =
    String.concat ""
    @@ List.flatten
         [ block_template_pre pre name; body; block_template_post post ]

  let compute_block_label cfg table v =
    let v_str = string_of_label v in
    let kw = Hashtbl.find table v in
    let pre = D.string_of_domain @@ Knowledge.pre kw in
    let post = D.string_of_domain @@ Knowledge.post kw in
    if v = ControlFlowGraph.entry_label cfg then
      block_template (Format.sprintf "ENTRY %s" v_str) pre post []
    else if v = ControlFlowGraph.exit_label cfg then
      block_template (Format.sprintf "EXIT %s" v_str) pre post []
    else
      let bb_kw = D.analyse_block cfg table v in
      let body =
        List.flatten
          [
            List.map visualise_instr (BlockKnowledge.body bb_kw);
            [ visualise_terminator (BlockKnowledge.terminator bb_kw) ];
          ]
      in
      block_template (Format.sprintf "BLOCK %s" v_str) pre post body

  let describe_vertex cfg table v =
    Format.sprintf "N%s[shape=none, margin=0, label=<%s>];" (string_of_label v)
      (compute_block_label cfg table v)

  let describe_outedges cfg v =
    let describe_edge w =
      Format.sprintf "N%s:x -> N%s:e;" (string_of_label v) (string_of_label w)
    in
    String.concat "\n" @@ List.map describe_edge
    @@ ControlFlowGraph.successors cfg v

  let visualize cfg table =
    let labels = ControlFlowGraph.labels cfg in
    let vertices =
      String.concat "\n" @@ List.map (describe_vertex cfg table) labels
    in
    let edges = String.concat "\n" @@ List.map (describe_outedges cfg) labels in
    String.concat "\n"
      [
        "digraph CFG {";
        "node [shape=none; fontname=\"Courier\" fontsize=\"9\"];";
        "ordering=out;";
        vertices;
        edges;
        "}";
      ]
end

module VisualiseRegGraph = struct
  let reg_to_name = function
    | REG_Hard i -> Format.sprintf "H%u" i
    | REG_Tmp i -> Format.sprintf "T%u" i
    | REG_Spec i -> Format.sprintf "S%u" i

  let describe_vertex v =
    Format.sprintf "%s[label=\"%s\"];" (reg_to_name v) (string_of_reg v)

  let describe_edge a b =
    Format.sprintf "%s -- %s;" (reg_to_name a) (reg_to_name b)

  let describe_vertices graph =
    String.concat "\n"
    @@ RegGraph.fold_vertex (fun r xs -> describe_vertex r :: xs) graph []

  let describe_edges graph =
    String.concat "\n"
    @@ RegGraph.fold_edges (fun a b xs -> describe_edge a b :: xs) graph []

  let visualise_graph reggraph =
    String.concat "\n"
      [
        "graph INF {";
        "layout=circo;";
        describe_vertices reggraph;
        describe_edges reggraph;
        "}";
      ]
end

module Lva_Graphviz = NgMakeGraphvizVisualizer (struct
  type domain = Analysis_domain.LiveVariables.domain

  let string_of_domain = Analysis_domain.LiveVariables.string_of_domain
end)

module Cfa_Graphviz = NgMakeGraphvizVisualizer (struct
  type domain = Analysis_domain.ConstantFolding.domain

  let string_of_domain = Analysis_domain.ConstantFolding.string_of_domain
end)

let visualize_live_variables = Lva_Graphviz.visualize
let visualize_interference_graph = VisualiseRegGraph.visualise_graph
let visualize_constant_folding = Cfa_Graphviz.visualize
