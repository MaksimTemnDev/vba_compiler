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
         | WhileStatement
         | DoWhileStatement
         ;

IfStmt: IF Expression THEN Statement END IF
	| IF Expression THEN Statement ELSE Statement 
	| IF Expression THEN TernarStatement
	| IF Expression THEN Statement ELSEIF Expression THEN Statement END IF
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