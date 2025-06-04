file=$1
shift
echo $file
cabal run sol-core -- -f $file $* &&
  cabal run yule -- output1.core &&
  forge script Output.sol
