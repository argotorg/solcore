file=$1
shift
echo $file
cabal run sol-core -- -f $file $* &&
  cabal run yule -- output.core &&
  forge script Output.sol
