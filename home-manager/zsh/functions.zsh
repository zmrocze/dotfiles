set -e

export PATH="$PATH:$HOME/.cargo/bin"

eval "$(zoxide init zsh)"
alias cd=z
alias ls=exa
alias ccat="bat -pp"
alias cat="bat --style=header,plain"
alias ps=procs
# alias rm=rip
alias -g mi="micro"
# alias sudo="doas"
alias -g json="python3 -m json.tool"
alias -g mapapply="xargs -L1"
alias hs="hsh"
alias -g grep="grep -E"
alias py=python
alias cp="cp -i"                                                # Confirm before overwriting something
alias df='df -h'                                                # Human-readable sizes
alias free='free -m'                                            # Show sizes in MB
alias lsblk="lsblk -o NAME,MAJ:MIN,RM,SIZE,RO,TYPE,MOUNTPOINTS,LABEL,FSTYPE"  # add LABEL,FSTYPE
alias -- l='ls -alh'
alias -- ll='ls -l'
# alias -- ls='ls --color=tty'
# not needed now
# alias config='/usr/bin/git --git-dir=$HOME/.cfg/.git/ --work-tree=$HOME'

dotfiles () {
	cl $HOME/dotfiles
	code .
}

map () {
    while read line;
    do 
        "$@" <<< $line;
    done
}

cl () {
	cd "$1" && \
	ls
}

cdir () {
	mkdir "$1" && \
	cd "$1"
}

# stev () {
# 	stack eval $1
# }

# status2add () {
# 	echo "$1" | hsh 'unwords . map last'
# }

findgrep () {
    args=()
    for x; do
        shift
        [[ $x == ::: ]] && break
        args+=("$x")
    done
    find "${args[@]}" -type f -exec grep -Hn "$@" {} \;
}

line() {
	if [ "$2" = '_' ]; then 
		xargs -I "$1" ${@[3,-1]}
	else 
		echo "expected '_'" && false
	fi
}

# 
# cipher='-aes-256-cbc -pbkdf2 -iter 2000000'
encrypt() {
	openssl enc -e -aes-256-cbc -pbkdf2 -iter 5000000 "$@"
}

decrypt() {
	openssl enc -d -aes-256-cbc -pbkdf2 -iter 5000000 "$@"
}

gitclonebare() {
	mkdir $2 && \
	git clone --bare $1 $2/.bare && \
	echo "gitdir: ./.bare" > $2/.git
}
