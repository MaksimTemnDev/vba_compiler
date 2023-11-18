%token END
%token WHILE DO LOOP UNTIL FOR TO STEP CONTINUE EXIT
%token IF THEN ELSE ELSEIF
%token TRUE FALSE
%token DIM
%token NEW AS RETURN NEXT
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
%token Function
%token Sub

%left '^'
%left UnarPlus UnarMinus
%right '*' '/'
%right '\\'
%right MOD
%right '+' '-'
%right '&'
%left BIT_LEFT_SHIFT BIT_RIGHT_SHIFT
%right '=' NOT_EQUAL '<' LESS_OR_SAME Is IsNot Like TypeOf
%left '>' MORE_OR_SAME
%left OR ORELSE
%left AND ANDALSO
%nonassoc '{' '}'

%%

Statement: EndList
	| Expression EndList
	| DimStmt
	| IfStmt
    | WhileStatement
    | DoLoopWhileStatement
	| DoLoopUntilStatement
	| ForStatement
	| FunctionDeclaration
    ;

DimStmt: DIM Declaration 
	| DIM IDENTIFIER '=' Expression
	| DIM IDENTIFIER '('TYPE_INTEGER')' AS Type
	| DIM IDENTIFIER '('TYPE_INTEGER')'
	;

IDENTIFIERlist: IDENTIFIERlist ',' IDENTIFIER
			  | IDENTIFIER
			  ;

Declaration: IDENTIFIERlist AS Type
		   | Declaration ',' Declaration
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
			   | NEW Type '('')' '{'StatementList'}'
               ;

StatementList: Statement
             | StatementList ',' Statement
             ;

BodyStmt: EndList StatementList RETURN Expression EndList END Function
		| EndList RETURN Expression END Function
		;
		
SubBobyStmt: EndList StatementList
		   | EndList
		   ;

ExpressionList: Expression
			  | ExpressionList ',' Expression

Expression: IDENTIFIER
		  | '('Expression')'
		  | IDENTIFIER '('ExpressionList')'
		  | IDENTIFIER '('')'
		  | Expression '=' EndList Expression
		  | Expression '+' EndList Expression
		  | Expression '&' EndList Expression
		  | Expression '-' EndList Expression
		  | Expression '/' EndList Expression
		  | Expression '*' EndList Expression
		  | Expression '\\' EndList Expression
		  | Expression MOD EndList Expression
		  | Expression '>' EndList Expression
		  | Expression '<' EndList Expression
		  | Expression MORE_OR_SAME EndList Expression
		  | Expression LESS_OR_SAME EndList Expression
		  | Expression NOT_EQUAL EndList Expression
		  | Expression BIT_LEFT_SHIFT EndList Expression
		  | Expression BIT_RIGHT_SHIFT EndList Expression
		  ;

FunctionDeclaration: Function IDENTIFIER '(' EndList ')' EndList BodyStmt
                   | Function IDENTIFIER '(' EndList ')' EndList AS Type EndList BodyStmt
                   | Function IDENTIFIER '(' EndList Declaration  EndList ')' EndList BodyStmt
                   | Function IDENTIFIER '(' EndList Declaration  EndList ')' EndList AS Type EndList BodyStmt
                   ;
				   
SubDeclaration: Sub IDENTIFIER '(' EndList ')' EndList BodyStmt
              | Sub IDENTIFIER '(' EndList Declaration  EndList ')' EndList BodyStmt
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
	
ArrayElementExpression: IDENTIFIER '('')'
                     ;
				 
EndList: TOKEN_LINE
	   | EndList TOKEN_LINE
	   ;