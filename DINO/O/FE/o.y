/* Simple translation of Oberon program.  */
Module : MODULE ident ';' OptImportList DeclSeq OptBeginStatementSeq EndModule
                                                         # module (1 3 4 5 6)
       | error ident ';' OptImportList DeclSeq OptBeginStatementSeq EndModule
                                                         # module (1 3 4 5 6)
       | error OptImportList DeclSeq OptBeginStatementSeq EndModule
                                                         # module (- 1 2 3 4)
       | error                                           # -
       ;
EndModule : END ident '.'                                # 1
          | error                                        # -
          ;
OptImportList :                                          # -
              | IMPORT ImportSeq ';'                     # 1
              ;
OptBeginStatementSeq :                                   # -
                     | BEGIN StatementSeq                # 1
                     ;
ImportSeq : Import                                       # conc (- 0)
          | ImportSeq ',' Import                         # conc (0 2)
          ;
Import : ident                                           # import (- 0)
       | ident ASSIGN ident                              # import (0 2)
       | error                                           # -
       ;
DeclSeq : CTVDeclSeq PDeclSeq                            # conc (0 1)
        ;
CTVDeclSeq :						 # -
           | CTVDeclSeq CONST ConstDeclSeq               # conc (0 2)
           | CTVDeclSeq TYPE TypeDeclSeq                 # conc (0 2)
           | CTVDeclSeq VAR VarDeclSeq                   # conc (0 2)
           | CTVDeclSeq error                            # 0
           ;
PDeclSeq :						 # -
         | PDeclSeq ProcDecl ';'                         # conc (0 1)
         | PDeclSeq ForwardDecl ';'                      # conc (0 1)
         | PDeclSeq error                                # 0
         ;
ConstDeclSeq :                                           # -
             | ConstDeclSeq ConstDecl                    # conc (0 1)
             ;
ConstDecl : IdentDef '=' ConstExpr ';'                   # const (1 0 2)
          ;
TypeDeclSeq :                                            # -
            | TypeDeclSeq TypeDecl                       # conc (0 1)
            ;
TypeDecl : IdentDef '=' Type ';'                         # type (1 0 2)
         ;
VarDeclSeq :                                             # -
           | VarDeclSeq VarDecl                          # conc (0 1)
           ;
VarDecl : IdentList ':' Type ';'                         # var (0 2)
        ;
ProcDecl : PROCEDURE Hint IdentDef OptFormalPars ';' DeclSeq
                     OptBeginStatementSeq EndProc        # proc (1 2 3 5 6 7)
         | error IdentDef OptFormalPars ';' DeclSeq
                     OptBeginStatementSeq EndProc        # proc (- 1 2 4 5 6)
         | error OptFormalPars ';' DeclSeq OptBeginStatementSeq EndProc
                                                         # proc (- - 1 3 4 5)
         | error NoFormalPars DeclSeq
                     OptBeginStatementSeq EndProc        # proc (- - 1 2 3 4)
         ;
Hint :                                                   # -
     | '*'                                               # hint
     ;
EndProc : END ident                                      # 1
        | error                                          # -
        ;
ForwardDecl : PROCEDURE '^' IdentDef OptFormalPars       # forward (0 2 3)
            ;
OptFormalPars : NoFormalPars                             # 0
              | '(' FPSectionSeq ')' ReturnType          # formals (0 1 3)
              | '(' ')' ReturnType                       # formals (0 - 2)
              ;
NoFormalPars :                                           # formals (- - -)
             ;
FPSectionSeq : FPSection                                 # conc (0 -)
             | FPSectionSeq ';' FPSection                # conc (0 2)
             ;
FPSection : IdentSeq ':' FormalType                      # fps (0 2)
          | VAR IdentSeq ':' FormalType                  # vfps (1 3)
          | error                                        # -
          ;
ReturnType  :                                            # -
            | ':' Qualident                              # 1
            ;
IdentSeq : ident                                         # conc (- 0)
         | IdentSeq ',' ident                            # conc (0 2)
         | error                                         # -
         | IdentSeq error                                # 0
         ;
FormalType : FormalType1                                 # 0
           | ProcedureType                               # 0
           ;
FormalType1 : Qualident                                  # 0
            | ARRAY OF FormalType1                       # oparray (0 2)
            ;
Type : Qualident                                         # 0
     | ARRAY ConstExprSeq OF Type                        # array (1 3)
     | RECORD FieldListSeq END                           # record (0 - 1)
     | RECORD '(' Qualident ')' FieldListSeq END         # record (0 2 4)
     | POINTER TO Type                                   # pointer (0 2)
     | ProcedureType                                     # 0
     ;
ProcedureType : PROCEDURE OptFormalPars                  # 1
              ;
ConstExprSeq : ConstExpr                                 # conc (- 0)
             | ConstExprSeq ',' ConstExpr                # conc (0 2)
             ;
FieldListSeq : FieldList                                 # conc (- 0)
             | FieldListSeq ';' FieldList                # conc (0 2)
             ;
FieldList :                                              # -
          | IdentList ':' Type                           # fields (0 2)
          | error                                        # -
          ;
StatementSeq : Statement                                 # conc (- 0)
             | StatementSeq ';' Statement                # conc (0 2)
             ;
Statement  :						 # -
           | Designator  ASSIGN  Expr                    # assign (1 0 2)
           | Designator                                  # call (0)
           | Designator '(' OptExprList ')'              # des_and_par (1 0 2)
           | IF CondSeq END                              # if (0 1 -)
           | IF CondSeq ELSE StatementSeq END            # if (0 1 3)
           | CASE Expr OF CaseSeq END                    # case (0 1 3 -)
           | CASE Expr OF CaseSeq ELSE StatementSeq END  # case (0 1 3 5)
           | WHILE Expr DO StatementSeq END              # while (0 1 3)
           | REPEAT StatementSeq UNTIL Expr              # repeat (0 3 1)
           | LOOP StatementSeq END                       # loop (0 1)
           | WITH Qualident ':' Qualident
               DO StatementSeq END                       # with (0 1 3 5)
           | EXIT                                        # exit (0)
           | RETURN                                      # return (0 -)
           | RETURN Expr                                 # return (0 1)
	   | error                                       # -
           ;
OptExprList :                                            # conc (- -)
            | ExprList                                   # 0
            ;
CondSeq : Cond                                           # conc (- 0)
        | CondSeq ELSIF Cond                             # conc (0 2)
        ;
Cond : Expr THEN StatementSeq                            # cond (0 2)
     ;
CaseSeq : Case                                           # conc (- 0)
        | CaseSeq '|' Case                               # conc (0 2)
        ;
Case :                                     # -
     | CaseLabelSeq ':' StatementSeq       # cases (0 2)
     ;
CaseLabelSeq : CaseLabels                  # conc (- 0)
             | CaseLabelSeq ',' CaseLabels # conc (0 2)
             ;
CaseLabels : ConstExpr                     # range (0 -)
           | ConstExpr RANGE ConstExpr     # range (0 2)
           ;
ConstExpr : Expr                           # 0
          ;
Expr : SimpleExpr                          # 0
     | SimpleExpr '=' SimpleExpr           # eq (1 0 2)
     | SimpleExpr '#' SimpleExpr           # ne (1 0 2)
     | SimpleExpr '<' SimpleExpr           # lt (1 0 2)
     | SimpleExpr LE  SimpleExpr           # le (1 0 2)
     | SimpleExpr '>' SimpleExpr           # gt (1 0 2)
     | SimpleExpr GE  SimpleExpr           # ge (1 0 2)
     | SimpleExpr IN  SimpleExpr           # in (1 0 2)
     | SimpleExpr IS  SimpleExpr           # is (1 0 2)
     | error                               # -
     ;
SimpleExpr : Term                          # 0
           | '+' Term                      # pos (0 1)
           | '-' Term                      # neg (0 1)
           | SimpleExpr '+' Term           # plus (1 0 2)
           | SimpleExpr '-' Term           # minus (1 0 2)
           | SimpleExpr OR Term            # or (1 0 2)
           ;
Term : Factor                              # 0
     | Term '*' Factor                     # mult (1 0 2)
     | Term '/' Factor                     # rdiv (1 0 2)
     | Term DIV Factor                     # div (1 0 2)
     | Term MOD Factor                     # mod (1 0 2)
     | Term '&' Factor                     # and (1 0 2)
     ;
Factor : Designator                        # 0
       | Designator '(' OptExprList ')'    # des_and_par (1 0 2)
       | number                            # 0
       | character                         # 0
       | string                            # 0
       | NIL                               # nil (0)
       | '{' '}'                           # set (0 -)
       | '{' SetEls '}'                    # set (0 1)
       | '(' Expr ')'                      # 1
       | '~' Factor                        # not (0 1)
       ;
SetEls : Element                           # conc (- 0)
       | SetEls ',' Element                # conc (0 2)
       ;
Element : Expr                             # range (0 -)
        | Expr RANGE Expr                  # range (0 2)
        ;
Designator : Qualident                     # 0
           | Designator '.' ident          # dot (1 0 2)
           | Designator '[' ExprList ']'   # index (1 0 2)
           | Designator '^'                # ref (1 0)
           | Designator '(' Qualident ')'  # des_and_par (1 0 2)
           ;
ExprList : Expr                            # conc (0 -)
         | ExprList ',' Expr               # conc (0 2)
         ;
IdentList : IdentDef                       # conc (- 0)
          | IdentList ',' IdentDef         # conc (0 2)
          ;
Qualident : ident                          # 0
          | ident '.' ident                # dot (1 0 2)
          ;
IdentDef : ident                           # 0
         | ident '*'                       # exportid (0)
         ;
