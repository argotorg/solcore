---
title:    'Specification for the experimental solidity'
author:   Argot collective
revision: 0.1
---


# 1. Introduction

This document describes the current specification of the 
experimental solidity (ES) programming language.
We start presenting some basic definitions and notations 
used. Next, we describe the syntax of the high-level 
language ... 

# 2. Basic definitions and notations

We let **x** denote an arbitrary identifier...

# 3. Design of the high-level language

The syntax of experimental solidity programs can be described by the 
following context-free grammar.

$$
  \begin{array}{lcl}
    Program   & \to & \overline{Decl} \\ 
    Decl      & \to & Fun\,|\,Data\,|\,Sym\,|\,Contract\,|\,Pragma\,|\,Class\,|\,Instance\\
    Fun       & \to & Sig\:Body\\
    Data      & \to & \mathbf{data}\:\mathbf{T}\:\overline{\alpha}\:Constrs\\
    Sym       & \to & \mathbf{type}\:\mathbf{S}\:\overline{\alpha}\,=\,\tau\\
    Contract  & \to & \mathbf{contract}\:\mathbf{D}\:\overline{\alpha}\:\overline{ContrDecl}\\
    Pragma    & \to & \mathbf{pragma}\:PragTy\:\overline{\mathbf{x}}\\
    Class     & \to & \mathbf{class}\:\overline{Pred}\Rightarrow\alpha\,:\,\mathbf{C}\,
                      \overline{\alpha}\:\mathbf{where}\:\overline{Sig}\\
    Instance  & \to & \mathbf{instance}\:\overline{Pred}\Rightarrow\tau\,:\,\mathbf{C}\,
                      \overline{\tau}\:\mathbf{where}\:\overline{Fun}\\
    Sig       & \to & \forall\,\overline{\alpha}\,.\,\overline{Pred}\,\Rightarrow\,\mathbf{function}\:\mathbf{F}\
                      (\overline{Arg}) \to \tau\\ 
    Body      & \to & \overline{Stmt}\\
    Constrs   & \to & \mathbf{=}\:\overline{Constr}\,|\,\lambda\\
    \tau      & \to & \alpha\,|\,\mathbf{T}\:\overline{\tau}\\
    ContrDecl & \to & Fun\,|\,FieldDecl\,|\,Constructor\\
    PragTy    & \to & \boldsymbol{boundedvar}\,|\,\boldsymbol{coverage}\,|\,
                      \boldsymbol{patterson}\\
    Pred      & \to & \tau\,:\,\mathbf{C}\,\overline{\tau}\\
    Arg       & \to & \mathbf{x}\,:\,\tau\,|\,\mathbf{x}\\
  \end{array}
$$
