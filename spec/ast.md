# Syntax for the high-level language

```
	CompilationUnit -> ImportList TopDeclList
	ImportList -> ImportList Import | \lambda
	Import -> 'import' QualName ';'                     
	TopDeclList -> TopDecl TopDeclList                  
	TopDeclList ->                                      
	TopDecl -> Contract                                 
	TopDecl -> Function                                 
	TopDecl -> ClassDef                                 
	TopDecl -> InstDef                                  
	TopDecl -> DataDef                                  
	TopDecl -> TypeSynonym                              
	Contract -> 'contract' Con OptParam '{' DeclList '}'    
	DeclList -> Decl DeclList                           
	DeclList ->                                         
	Decl -> FieldDef                                    
	Decl -> DataDef                                     
	Decl -> Function                                    
	Decl -> Constructor                                 
	Decl -> TypeSynonym                                 
	TypeSynonym -> 'type' Name '=' Type                 
	FieldDef -> Name ':' Type InitOpt ';'               
	DataDef -> 'data' Con OptParam '=' Constrs          
	Constrs -> Constr '|' Constrs                       
	Constrs -> Constr                                   
	Constr -> Con OptTypeParam                          
	ClassDef -> 'class' ContextOpt Var ':' Con OptParam ClassBody    
	ClassBody -> '{' Signatures '}'                     
	OptParam -> '[' VarCommaList ']'                    
	OptParam ->                                         
	VarCommaList -> Var ',' VarCommaList                
	VarCommaList -> Var                                 
	ContextOpt ->                                       
	ContextOpt -> Context                               
	Context -> '[' ConstraintList ']' '=>'              
	ConstraintList -> Constraint ',' ConstraintList     
	ConstraintList -> Constraint                        
	Constraint -> Type ':' Con OptTypeParam             
	Signatures -> Signature ';' Signatures              
	Signatures ->                                       
	Signature -> 'function' Name ConOpt '(' ParamList ')' OptRetTy    
	ConOpt ->                                           
	ConOpt -> '[' ConstraintList ']'                    
	ParamList -> Param                                  
	ParamList -> Param ',' ParamList                    
	ParamList ->                                        
	Param -> Name ':' Type                              
	Param -> Name                                       
	InstDef -> 'instance' ContextOpt Type ':' Con OptTypeParam InstBody    
	OptTypeParam -> '[' TypeCommaList ']'               
	OptTypeParam ->                                     
	TypeCommaList -> Type ',' TypeCommaList             
	TypeCommaList -> Type                               
	Functions -> Function Functions                     
	Functions ->                                        
	InstBody -> '{' Functions '}'                       
	Function -> Signature Body                          
	OptRetTy -> '->' Type                               
	OptRetTy ->                                         
	Constructor -> 'constructor' '(' ParamList ')' Body    
	Body -> '{' StmtList '}'                            
	StmtList -> Stmt ';' StmtList                       
	StmtList ->                                         
	Stmt -> Expr '=' Expr                               
	Stmt -> 'let' Name ':' Type InitOpt                 
	Stmt -> 'let' Name InitOpt                          
	Stmt -> Expr                                        
	Stmt -> 'return' Expr                               
	Stmt -> 'match' MatchArgList '{' Equations '}'      
	Stmt -> AsmBlock                                    
	MatchArgList -> Expr                                
	MatchArgList -> Expr ',' MatchArgList               
	InitOpt ->                                          
	InitOpt -> '=' Expr                                 
	Expr -> Name                                        
	Expr -> Con ConArgs                                 
	Expr -> Literal                                     
	Expr -> '(' Expr ')'                                
	Expr -> Expr '.' Name                               
	Expr -> Expr '.' Name FunArgs                       
	Expr -> Name FunArgs                                
	Expr -> 'lam' '(' ParamList ')' Body                
	ConArgs -> '[' ExprCommaList ']'                    
	ConArgs ->                                          
	FunArgs -> '(' ExprCommaList ')'                    
	ExprCommaList -> Expr                               
	ExprCommaList ->                                    
	ExprCommaList -> Expr ',' ExprCommaList             
	Equations -> Equation Equations                     
	Equations ->                                        
	Equation -> '|' PatCommaList '=>' StmtList          
	PatCommaList -> Pattern                             
	PatCommaList -> Pattern ',' PatCommaList            
	Pattern -> Name                                     
	Pattern -> Con PatternList                          
	Pattern -> '_'                                      
	Pattern -> Literal                                  
	Pattern -> '(' Pattern ')'                          
	PatternList -> '[' PatList ']'                      
	PatternList ->                                      
	PatList -> Pattern                                  
	PatList -> Pattern ',' PatList                      
	Literal -> number                                   
	Literal -> stringlit                                
	Type -> Con OptTypeParam                            
	Type -> Var                                         
	Type -> LamType                                     
	LamType -> '(' TypeCommaList ')' '->' Type          
	Var -> Name                                         
	Con -> tycon                                        
	QualName -> Con                                     
	QualName -> QualName '.' Con                        
	Name -> identifier                                  
	AsmBlock -> 'assembly' YulBlock                     
	YulBlock -> '{' YulStmts '}'                        
	YulStmts -> YulStmt OptSemi YulStmts                
	YulStmts ->                                         
	YulStmt -> YulAssignment                            
	YulStmt -> YulBlock                                 
	YulStmt -> YulVarDecl                               
	YulStmt -> YulExp                                   
	YulStmt -> YulIf                                    
	YulStmt -> YulSwitch                                
	YulStmt -> YulFor                                   
	YulStmt -> 'continue'                               
	YulStmt -> 'break'                                  
	YulStmt -> 'leave'                                  
	YulFor -> 'for' YulBlock YulExp YulBlock YulBlock    
	YulSwitch -> 'switch' YulExp YulCases YulDefault    
	YulCases -> YulCase YulCases                        
	YulCases ->                                         
	YulCase -> 'case' YulLiteral YulBlock               
	YulDefault -> 'default' YulBlock                    
	YulDefault ->                                       
	YulIf -> 'if' YulExp YulBlock                       
	YulVarDecl -> 'let' IdentifierList YulOptAss        
	YulOptAss -> ':=' YulExp                            
	YulOptAss ->                                        
	YulAssignment -> IdentifierList ':=' YulExp         
	IdentifierList -> Name                              
	IdentifierList -> Name ',' IdentifierList           
	YulExp -> YulLiteral                                
	YulExp -> Name                                      
	YulExp -> Name YulFunArgs                           
	YulFunArgs -> '(' YulExpCommaList ')'               
	YulExpCommaList -> YulExp                           
	YulExpCommaList ->                                  
	YulExpCommaList -> YulExp ',' YulExpCommaList       
	YulLiteral -> number                                
	YulLiteral -> stringlit                             
	OptSemi -> ';'                                      
	OptSemi ->                                          
```


