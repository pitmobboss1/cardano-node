help: ## Print documentation
	@{ grep -hE '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST); echo -e '$(EXTRA_HELP)'; } | sed 's/^ //' | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-33s\033[0m %s\n", $$1, $$2}'

include lib.mk
include nix.mk

PROJECT_NAME = cardano-node
NUM_PROC     = $(nproc --all)

## One of:  shey alra mary alzo bage
ERA     ?= bage

PROFILE ?= default-${ERA}
BACKEND ?= supervisor
REV     ?= master
ITER    ?=
ARGS    ?=
CMD     ?=
RUN     ?=

lint hlint: ## Run the CI version of hlint
	nix build --no-link '.#checks/hlint' --cores 0
haddock-hoogle haddocks hoogle:
	if test -z "$$IN_NIX_SHELL"; then nix-shell --run 'cabal update && cabal haddock all --haddock-hoogle'; else cabal update && cabal haddock all --haddock-hoogle; fi
host-hlint: ## Run the system (not Nix) version of hlint
	hlint bench cardano-{api,cli,node,node-capi,node-chairman,submit-api,testnet,tracer}

# This modifies all Haskell source files in-place for coding style
# compliance using the stylish-haskell tool.
stylish-haskell:
	@: The top-level directory for the project.;			\
	REPO_TOP_DIR=`git rev-parse --show-toplevel`;			\
									\
	: The YAML configuration file for stylish-haskell.;		\
	SH_YAML=$${REPO_TOP_DIR}/.stylish-haskell.yaml;			\
									\
	: Set the return code variable to a default of success.		\
	: The global scope doesn\'t actually help the two groups	\
	: communicate because the pipe demands subshells. So it		\
	: has to be communicated over file descriptors.;		\
	rc=0;								\
									\
	: Get free file descriptors to pass the exit code between	\
	: subshells and a temporary fd to swap stdout and stderr.;	\
	eval "exec {rcfd}<> <(:)";					\
	eval "exec {swpfd}<> <(:)";					\
	{								\
		: Starting from all the cabal files in the project,	\
		: found via git, this descends into the directory	\
		: containing the file so that stylish-haskell can	\
		: find it. Its search strategy is to ascend from	\
		: its current directory at the time of invocation,	\
		: not to scrutinise the path of the file given. So	\
		: this strategy is required to accommodate it. And	\
		: it needs the cabal files for language extensions	\
		: affecting parsing for code reformatting.;		\
		for cabal_file in `git ls-files '*.cabal'`;		\
		do							\
			cd $${REPO_TOP_DIR}/`dirname $${cabal_file}`;	\
			stylish-haskell -c $${SH_YAML}			\
					-i				\
					`git ls-files '*.hs' '*.lhs'`;	\
			shrc=$${?};					\
			if [ $${shrc} -ne 0 ];				\
			then						\
				rc=$${shrc};				\
				break;					\
			fi;						\
		done;							\
									\
		: Pass the exit code through one anonyous fd.;		\
		echo "$${rc}" 1>&$${rcfd};				\
									\
	: Swap stdout and stderr to post-process stderr via the other.;	\
	} {swpfd}>&1 1>&2 2>&$${swpfd}					\
	| {								\
		: read tokenises according to IFS, so that changing	\
		: IFS to be stylish-haskell\'s delimiter for the	\
		: file name at the beginning of error report lines	\
		: will yield the file name to be re-relativised to	\
		: be relative to the top directory of the project.;	\
		OLDIFS=$${IFS};						\
		IFS=":";						\
		while read fpath rest;					\
		do							\
			: git happily has utility functions for		\
			: both carrying out a search to check		\
			: whether a string is a path to a		\
			: git-controlled file and re-relativising	\
			: file path names relative to the project	\
			: root. If we discover that it is under		\
			: git\'s control, the rewritten path gets	\
			: put into the reassembled stderr line.;	\
			if git ls-files --error-unmatch			\
					  "*/$${fpath}"			\
						> /dev/null 2>&1;	\
			then						\
				fpath=`git ls-files			\
					    --full-name			\
					    "*/$${fpath}"`;		\
			fi;						\
			IFS="$${OLDIFS}";				\
			echo "$${fpath}:$${rest}";			\
			IFS=":";					\
			done;						\
									\
	  : Swap stdout and stderr back to normal.;			\
	  } {swpfd}>&1 1>&2 2>&$${swpfd};				\
	read -u $${rcfd} rc;						\
	exit $${rc};

cabal-hashes:
	nix run .#checkCabalProject

cli node:
	cabal --ghc-options="+RTS -qn8 -A32M -RTS" build cardano-$@

trace-documentation:
	cabal run -- exe:cardano-node trace-documentation --config 'configuration/cardano/mainnet-config-new-tracing.yaml' --output-file 'doc/new-tracing/tracers_doc_generated.md'

###
### Workbench
###
workbench-ci: workbench-ci-test ci-test-auto ci-test-autonix ci-test-autonomadpodman
CI_TARGETS := hlint workbench-ci haddock-hoogle
ci:  ci-report ci-targets
ci-report:
	@echo -e "\033[34mGoals under test\033[0m:  \033[33m$(CI_TARGETS)\033[0m"
ci-targets:  $(CI_TARGETS)

workbench-internals-walkthrough:
	emn nix/workbench/doc.org

##
## Base targets:
##
shell:                                           ## Nix shell, (workbench from /nix/store), vars: PROFILE, CMD, RUN
	nix-shell -A 'workbench-shell' --max-jobs 8 --cores 0 --show-trace --argstr profileName ${PROFILE} --argstr backendName ${BACKEND} ${ARGS} ${if ${CMD},--command "${CMD}"} ${if ${RUN},--run "${RUN}"}
shell-dev shell-prof shell-nix: shell
shell-nix: ARGS += --arg 'useCabalRun' false ## Nix shell, (workbench from Nix store), vars: PROFILE, CMD, RUN
shell-prof: ARGS += --arg 'profiling' '"space"'  ## Nix shell, everything Haskell built profiled

analyse: RUN := wb analyse std ${TAG}
analyse: shell

list-profiles:                                   ## List workbench profiles
	nix build .#all-profiles-json && cat result
show-profile:                                    ## NAME=profile-name
	@test -n "${NAME}" || { echo 'HELP:  to specify profile to show, add NAME=profle-name' && exit 1; }
	nix build .#all-profiles-json --json --option substitute false | jq '.[0].outputs.out' -r | xargs jq ".\"${NAME}\" | if . == null then error(\"\n###\n### Error:  unknown profile: ${NAME}  Please consult:  make list-profiles\n###\") else . end"
ps:                                              ## Plain-text list of profiles
	@nix build .#workbench.profile-names-json --json | jq '.[0].outputs.out' -r | xargs jq '.[]' --raw-output

##
## Profile-based cluster shells (autogenerated targets)
##
PROFILES_BASE             := default plutus plutus-secp-ecdsa plutus-secp-schnorr oldtracing idle tracer-only
PROFILES_FAST             := fast fast-p2p fast-plutus fast-notracer fast-oldtracing
PROFILES_CI_TEST          := ci-test ci-test-p2p ci-test-plutus ci-test-notracer ci-test-rtview ci-test-dense10
PROFILES_CI_BENCH         := ci-bench ci-bench-p2p ci-bench-plutus ci-bench-plutus-secp-ecdsa ci-bench-plutus-secp-schnorr ci-bench-notracer ci-bench-rtview
PROFILES_TRACE_BENCH      := trace-bench trace-bench-notracer trace-bench-oldtracing trace-bench-rtview
PROFILES_EPOCHTRANS       := epoch-transition
PROFILES_PLUTUSCALL       := plutuscall-loop-plain plutuscall-secp-ecdsa-plain plutuscall-secp-schnorr-plain
PROFILES_PLUTUSCALL       += plutuscall-loop-half plutuscall-secp-ecdsa-half plutuscall-secp-schnorr-half
PROFILES_PLUTUSCALL       += plutuscall-loop-double plutuscall-secp-ecdsa-double plutuscall-secp-schnorr-double
PROFILES_MODEL		        := model-value model-secp-ecdsa-plain model-secp-ecdsa-half model-secp-ecdsa-double
PROFILES_MODEL		       	+= model-value-test
PROFILES_10               := 10 10-p2p 10-plutus 10-notracer
PROFILES_FORGE_STRESS     := forge-stress forge-stress-p2p forge-stress-plutus forge-stress-plutus-solo forge-stress-notracer forge-stress-large forge-stress-solo forge-stress-light
PROFILES_FORGE_STRESS_PRE := forge-stress-pre forge-stress-pre-plutus forge-stress-pre-notracer forge-stress-pre-solo
PROFILES_FORGE_STRESS_RTS := forge-stress-pre-rtsA4m forge-stress-pre-rtsA64m forge-stress-pre-rtsN3 forge-stress-pre-rtsA4mN3 forge-stress-pre-rtsA64mN3
PROFILES_CHAINSYNC        := chainsync-early-byron  chainsync-early-byron-notracer  chainsync-early-byron-oldtracing
PROFILES_CHAINSYNC        += chainsync-early-alonzo chainsync-early-alonzo-notracer chainsync-early-alonzo-oldtracing chainsync-early-alonzo-p2p
PROFILES_VENDOR           := dish dish-plutus dish-10M dish-10M-plutus
# "qa" and "perf" namespaces for cardano world (world.dev.cardano.org) Nomad
# Not all local profiles are compatible (yet) with a cloud run
# Cloud version of "default", "ci-test" and "ci-bench"
PROFILES_CW_QA            := default-cw-qa ci-test-cw-qa ci-bench-cw-qa
# The 52+explorer profile
PROFILES_CW_PERF          := default-cw-perf ci-test-cw-perf ci-bench-cw-perf cw-perf-value

LOCAL_PROFILES += $(PROFILES_BASE)
LOCAL_PROFILES += $(PROFILES_FAST)
LOCAL_PROFILES += $(PROFILES_CI_TEST)
LOCAL_PROFILES += $(PROFILES_CI_BENCH)
LOCAL_PROFILES += $(PROFILES_TRACE_BENCH)
LOCAL_PROFILES += $(PROFILES_EPOCHTRANS)
LOCAL_PROFILES += $(PROFILES_PLUTUSCALL)
LOCAL_PROFILES += $(PROFILES_MODEL)
LOCAL_PROFILES += $(PROFILES_10)
LOCAL_PROFILES += $(PROFILES_FORGE_STRESS)
LOCAL_PROFILES += $(PROFILES_FORGE_STRESS_PRE)
LOCAL_PROFILES += $(PROFILES_FORGE_STRESS_RTS)
LOCAL_PROFILES += $(PROFILES_CHAINSYNC)
LOCAL_PROFILES += $(PROFILES_VENDOR)
CLOUD_PROFILES += $(PROFILES_CW_QA) $(PROFILES_CW_PERF)

## Note:  to enable a shell for a profile, just add its name (one of names from 'make ps') to SHELL_PROFILES

$(eval $(call define_profile_targets,           $(LOCAL_PROFILES)))
$(eval $(call define_profile_targets_nomadcloud,$(CLOUD_PROFILES)))

###
### Misc
###
clean-profile proclean:
	rm -f *.html *.prof *.hp *.stats *.eventlog

clean: clean-profile
	rm -rf logs/ socket/ cluster.*

full-clean: clean
	rm -rf db dist-newstyle $(shell find . -name '*~' -or -name '*.swp')

cls:
	echo -en "\ec"

.PHONY: cabal-hashes clean cli cls cluster-profiles help node run-test shell shell-dev stylish-haskell $(LOCAL_PROFILES) workbench-ci-test
