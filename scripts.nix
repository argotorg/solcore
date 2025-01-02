{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:sol-core' --allow-eval --warnings";
  testScript = s "test" "cabal run test:sol-core-tests";
  hoogleScript = s "hgl" "hoogle serve";
}
