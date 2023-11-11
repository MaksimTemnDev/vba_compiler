%token END
%token WHILE DO LOOP UNTIL FOR TO STEP CONTINUE EXIT
%token IF THEN ELSE ELSEIF
%token TRUE FALSE
%token DIM
%token NEW
%token IDENTIFIER
%token TYPE_BOOLEAN
%token TYPE_BYTE
%token TYPE_INTEGER
%token TYPE_SINGLE
%token TYPE_SHORT
%token TYPE_DOUBLE
%token TYPE_DECIMAL
%token TYPE_DATE
%token TYPE_CHAR
%token TYPE_STRING
%token TYPE_OBJECT
%token TOKEN_LINE

%left '^'
%left UnarPlus UnarMinus
%right '*' '/'
%right '\'
%right 'Mod'
%right '+' '-'
%right '&'
%left '<<' '>>'
%right '=' '<>' '<' '<=' Is IsNot Like TypeOf..Is
%left '>' '>='
%left OR ORELSE
%left AND ANDALSO
%nonassoc '{' '}'

%%

Statement: Statement
	| Expression Statement
	| DimStmt Statement
	| IfStmt Statement
    | StatementErase
    | BaseExpression StatementErase
    | DimStmt StatementErase
    | WhileStatement StatementErase
    | DoLoopWhileStatement StatementErase
	| DoLoopUntilStatement StatementErase
	| ForStatement StatementErase
    ;

DimStmt: DIM IDENTIFIERlist Declaration
	| DIM IDENTIFIER '=' Expression
	| DIM IDENTIFIER '('TYPE_INTEGER')' AS Type
	;

IDENTIFIERlist: IDENTIFIERlist ',' IDENTIFIER
			  | IDENTIFIER
			  ;

Declaration: AS Type
		   | IDENTIFIERlist Declaration
		   ;

Type: TYPE_BOOLEAN
	| TYPE_BYTE
	| TYPE_INTEGER
	| TYPE_SINGLE
	| TYPE_SHORT
	| TYPE_DOUBLE
	| TYPE_DECIMAL
	| TYPE_DATE
	| TYPE_CHAR
	| TYPE_STRING
	| TYPE_OBJECT
	;

ArrayStatement: '{' StatementList '}'
               | '{' '}'
			   | Statement '('')' AS Type
			   | NewStatement Type '('')' '{'StatementList'}'
               ;

StatementList: Statement
             | StatementList ',' Statement
             ;

ExpressionList: Expression
			  | ExpressionList ',' Expression

Expression:
		  | IDENTIFIER
		  | '('Expression')'
		  | IDENTIFIER '('ExpressionList')'
		  | IDENTIFIER '('')'
		  | Expression '=' EndList Expression
		  | Expression '+' EndList Expression
		  | Expression '-' EndList Expression
		  | Expression '/' EndList Expression
		  | Expression '*' EndList Expression
		  | Expression '\' EndList Expression
		  | Expression 'Mod' EndList Expression
		  | Expression '>' EndList Expression
		  | Expression '<' EndList Expression
		  | Expression '>=' EndList Expression
		  | Expression '=<' EndList Expression
		  | Expression '<>' EndList Expression
		  | Expression '<<' EndList Expression
		  | Expression '>>' EndList Expression
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
	
ArrayElementStatement: Statement '(' Statement ')'
                     ;

NewStatement: New
	        ;
			
writeLineStatement: writeLine '(' StatementList ')'
				  : writeLine '(' StatementErase ')'
				  ;
				 
writeStatement: write '(' StatementList ')'
			  ;

readLineStatement: readLine '(' ')'
				 ;
				 
EndList: TOKEN_LINE
	   | EndList TOKEN_LINE
	   ;