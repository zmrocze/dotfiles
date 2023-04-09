# Audit

## Methodology

MLabs analysed the validator scripts from the Indigo git remote
[IndigoProtocol/smart-contracts](https://github.com/IndigoProtocol/smart-contracts)
starting with commit `bcf9ed05`. The base commit was later updated to
commit `82e69a21` at the request of the client. During the process the
found issues and vulnerabilities were posted directly to the repository and
filed as issues tracked under a meta \emph{Audit Issue} with number:
[#439](https://github.com/IndigoProtocol/smart-contracts/issues/439).
Some of the findings were accompanied by additional tests, or proofs of
behaviour to the aforementioned repository.

MLabs spent the first sprint in an exploratory phase of the protocol,
looking across all modules provided, with all auditors inspecting all
modules. This exploratory phase was followed by a more structured phase,
in which auditors were assigned specific modules to inspect in detail
and create local audit reports, presented in section
\protect\hyperlink{22-module-specific-analyis}{2.2 Module specific
analyis}.

To leverage a standardised metric for the severity of the open standard
[Common Vulnerability Scoring System](https://www.first.org/cvss/),
together with the [NVD Calculator](https://nvd.nist.gov/vuln-metrics/cvss/v3-calculator). 
The metrics from the mentioned tools were included with
each vulnerability. MLabs recognises that some of the parameters are not
conclusive for the protocol - but considers that leveraging such a
standard is still valuable to offer a more unbiased severity metric to
found vulnerabilities.

## Executive Summary

% A run through of all the found vulnerabilties - outlining sort of the severity
% and linking them with their actual description. Underlining impact and
% giving an overview of the total findings.

## Coverage

Although these are the vulnerabilties that we have found, the team have actually
looked at more attack vectors, which can be found in the
\ref{vulnerability-types}{vulnerability types table}.


## Vulnerabilities

!INCLUDE "vulnerabilities/cdp-liquidation-incorrect-logic.md", 2
!INCLUDE "vulnerabilities/cdp-liquidation-missing-incentive.md", 2
!INCLUDE "vulnerabilities/cdp-other-redeemer.md",2
!INCLUDE "vulnerabilities/collector-tests.md", 2
!INCLUDE "vulnerabilities/collector-validator.md",2
!INCLUDE "vulnerabilities/documentation-enhancements.md", 2
!INCLUDE "vulnerabilities/locked-ada-values.md",2
!INCLUDE "vulnerabilities/missing-integration-tests.md", 2
!INCLUDE "vulnerabilities/poll-other-redeemer.md",2
!INCLUDE "vulnerabilities/proposal-exploit.md", 2
!INCLUDE "vulnerabilities/stability-pool-contention.md",2
