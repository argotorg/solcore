These 23 `.sol` files are copied from the successful syntax fixtures in
`solcore-rs`, branch `new-syntax`, revision `59f11626`:
`crates/parser/tests/fixtures/ok/`.

`ReferenceSyntaxTests` parses each source, prints it in canonical syntax, then
parses the output and compares the syntax trees. The focused `NewSyntax*Tests`
cover additional grammar boundaries and rejection of removed syntax.
