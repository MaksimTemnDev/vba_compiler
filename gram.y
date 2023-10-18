%{

%}

%%

Statement: Statement
	| Expression Statement
	| DimStmt Statement
	| IfStmt Statement
        ;

DimStmt: DIM Declaration
	| DIM '=' Expression
	;

ArrayStatement: '{' StatementList '}'
               | '{' '}'
               ;

StatementList: Statement
             | StatementList Statement
             ;
       
Statement: StatementErase
         | BaseExpression StatementErase
         | DimStmt StatementErase
         | WhileStatement StatementErase
         | DoLoopWhileStatement StatementErase
	 | DoLoopUntilStatement StatementErase
	 | ForStatement StatementErase
         ;

IfStmt: IF Expression THEN Statement END IF
	| IF Expression THEN Statement ELSE Statement 
	| IF Expression THEN TernarStatement
	| IF Expression THEN Statement ELSEIF Expression THEN Statement END IF
	;

WhileStatement: WHILE Expression Statement END WHILE
	| WHILE IF Expression THEN Statement CONTINUE WHILE END IF END WHILE
	| WHILE IF Expression THEN Statement EXIT WHILE END IF END WHILE
	;

DoLoopUntilStatement: DO Statement LOOP UNTIL Expression
	;

DoLoopWhileStatement: DO WHILE Expression Statement LOOP
	| DO WHILE Expression IF Expression THEN EXIT DO END IF Statement LOOP
	| DO WHILE Expression IF Expression THEN CONTINUE DO END IF Statement LOOP
	;	

ForStatement: FOR Statement '=' Statement TO Statement Statement NEXT
	| FOR Statement '=' Statement TO Statement STEP Statement Statement NEXT
	| FOR Statement '=' Statement TO Statement IF Expression THEN CONTINUE FOR END IF NEXT
	| FOR Statement '=' Statement TO Statement IF Expression THEN EXIT FOR END IF NEXT
	;

%right ‘=’
%left ’-’,’+’
%left ‘*’,’/’,’%’
%left ‘.’
%left ‘[’,’]’
%left UMINUS
%nonasoc‘(’,’)’
%%
expr: CONST_INT
        | CONST_STRING
        | CONST_DOUBLE
        | CONST_FLOAT
        | ID
        | expr ‘+’ expr
        | expr ‘-’ expr
        | expr ‘*’ expr
        | expr ‘-’
        | expr ‘/’ expr
        | expr ‘=’ ‘(’ ’[‘ expr‘]’’ )’
        | ‘(‘ expr‘ )’
        | ‘[’expr’]’
        | expr’.’ID
        | ID’(’expr-list’)’
        | expr’:’ID’(’expr-list’)’
        | ‘-’expr ‘%’ prec UMINUS

%%