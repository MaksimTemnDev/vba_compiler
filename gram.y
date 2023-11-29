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
%token Iif
%token KW_STATIC

%token STRING
%token DECIMAL_NUMBER
%token BYTE_NUMBER
%token DOUBLE
%token ARRAY_ELEMENT_ACCESS_OPERATOR
%token BOOLEAN
%token SINGLE
%token SHORT
%token DATE
%token CHAR
%token OBJECT

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

GlobalCode: FunctionDeclaration
		  | SubDeclaration
		  | DimStmt
		  ;

Statement: DimStmt
	| IfStmt
    | WhileStatement
    | DoLoopWhileStatement
	| DoLoopUntilStatement
	| ForStatement
	| ArrayStatement
	| StaticStmt
    ;

DimStmt: DIM DimSingle
	   | DIM DimArray
	   ;

DimSingle: IDENTIFIERlist '=' Expression
	| IDENTIFIERlist AS Type
	;

DimArray: ArrayIDdeclaration
	    | ArrayIDdeclaration AS Type
		;

IDENTIFIERlist: IDENTIFIER
 			  | IDENTIFIERlist ',' IDENTIFIER
			  ;

StaticStmt: KW_STATIC DimSingle
	      | KW_STATIC DimArray
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

ArrayBody: IDENTIFIERlist
		 | '{' '}'
		 ;

ArrayStatement: '{' ArrayBody '}'
               | '{' '}'
			   | NEW Type '('')' '{'ArrayBody'}'
               ;

ArrayIDdeclaration: ArrayElementExpression
				  | ArrayIDdeclaration ',' ArrayElementExpression
				  ;

StatementList: Statement EndList
             | StatementList ',' Statement EndList
             ;

BodyStmt: StatementList RETURN Expression EndList END Function
		| RETURN Expression END Function
		;
		
SubBobyStmt: EndList StatementList
		   |
		   ;

ExpressionList: Expression
			  | ExpressionList ',' Expression
			  ;

Expression: ArrayElementExpression
		  | Expression '=' EndList Expression
		  | Expression '+' EndList Expression
		  | Expression '&' EndList Expression
		  | Expression '-' EndList Expression
		  | Expression '/' EndList Expression
		  | Expression '*' EndList Expression
		  | Expression '\\' EndList Expression
		  | Expression MOD EndList Expression
		  | IDENTIFIER '('ExpressionList')'
		  | Expression '>' EndList Expression
		  | Expression '<' EndList Expression
		  | Expression MORE_OR_SAME EndList Expression
		  | Expression LESS_OR_SAME EndList Expression
		  | Expression NOT_EQUAL EndList Expression
		  | Expression BIT_LEFT_SHIFT EndList Expression
		  | Expression BIT_RIGHT_SHIFT EndList Expression
		  | '('Expression')'
		  | TernarOperator
		  ;
		  

FunctionDeclaration: Function IDENTIFIER '(' EndList ')' EndList BodyStmt
                   | Function IDENTIFIER '(' EndList ')' EndList AS Type EndList BodyStmt
                   | Function IDENTIFIER '(' EndList IDENTIFIERlist  EndList ')' EndList BodyStmt
                   | Function IDENTIFIER '(' EndList IDENTIFIERlist  EndList ')' EndList AS Type EndList BodyStmt
                   ;
				   
SubDeclaration: Sub IDENTIFIER '(' EndList ')' SubBobyStmt
              | Sub IDENTIFIER '(' EndList IDENTIFIERlist  EndList ')' SubBobyStmt
              ;

IfStmt: IF Expression THEN EndList Statement EndList END IF
	| IF Expression THEN EndList Statement EndList ELSE EndList Statement 
	| IF Expression THEN EndList TernarOperator
	| IF Expression THEN EndList Statement EndList ELSEIF Expression THEN EndList Statement EndList END IF
	;

TernarOperator: Iif '('Expression ',' Expression ',' Expression')'

WhileStatement: WHILE Expression Statement END WHILE
	| WHILE IF Expression THEN Statement CONTINUE WHILE END IF END WHILE
	| WHILE IF Expression THEN Statement EXIT WHILE END IF END WHILE
	;

DoLoopUntilStatement: DO EndList ExpressionList EndList LOOP UNTIL Expression TOKEN_LINE
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