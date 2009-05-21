GHC_DIR=~/Programming/Checkouts/ghc.transfer2/ghc/stage2-inplace

.PHONY : pluginstuff peeling-core unrolling-core all

pluginstuff :
	cd ../library && cabal-inplace $(GHC_DIR) install
	cd ../plugin && cabal-inplace $(GHC_DIR) install

peeling-core : pluginstuff
	$(GHC_DIR)/ghc -fforce-recomp -O --make Peeling.hs -plg LoopUnrolling.Plugin -dverbose-core2core > Peeling.core

unrolling-core : pluginstuff
	$(GHC_DIR)/ghc -fforce-recomp -O --make Unrolling.hs -plg LoopUnrolling.Plugin -dverbose-core2core > Unrolling.core

all : peeling-core unrolling-core