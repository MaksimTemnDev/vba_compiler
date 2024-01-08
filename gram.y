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
%right '=' NOT_EQUAL '<' LESS_OR_SAME Is IsNot Like TypeOf Not
%left '>' MORE_OR_SAME
%left OR ORELSE
%left AND ANDALSO
%nonassoc '{' '}'

%%

GlobalCodeList: GlobalCode
			  | GlobalCodeList GlobalCode
			  ;

GlobalCode: FunctionDeclaration
		  | SubDeclaration
		  | DimStmt
		  ;

Statement: DimStmt EndList
 		 | IfStmt EndList
   		 | WhileStatement EndList
		 | DoLoopWhileStatement EndList
		 | DoLoopUntilStatement EndList
		 | ForStatement EndList
		 | StaticStmt EndList
		 | Expression EndList
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

IDENTIFIERlist: IDENTIFIEREndl
 			  | IDENTIFIERlist ',' IDENTIFIEREndl
			  ;
			  
IDENTIFIEREndl: IDENTIFIER OptEndl

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

StatementList: Statement
             | StatementList Statement
             ;

BodyStmt: StatementList RETURN Expression EndList END Function EndList
		| RETURN Expression END Function EndList
		;
		

ExpressionList: Expression
			  | ExpressionList ',' Expression
			  ;

Expression: ArrayElementExpression
		  | Expression '=' OptEndl Expression
		  | Expression '+' OptEndl Expression
		  | Expression '&' OptEndl Expression
		  | Expression '-' OptEndl Expression
		  | Expression '/' OptEndl Expression
		  | Expression '*' OptEndl Expression
		  | Expression '\\' OptEndl Expression
		  | Expression MOD OptEndl Expression
		  | IDENTIFIER '('ExpressionList')'
		  | Expression '>' OptEndl Expression
		  | Expression '<' OptEndl Expression
		  | Expression MORE_OR_SAME OptEndl Expression
		  | Expression LESS_OR_SAME OptEndl Expression
		  | Expression NOT_EQUAL OptEndl Expression
		  | Expression BIT_LEFT_SHIFT OptEndl Expression
		  | Expression BIT_RIGHT_SHIFT OptEndl Expression
		  | UnarExpr
		  | ArrayStatement
		  | '('Expression')'
		  | TernarOperator
		  | SHORT
		  | SINGLE
		  | BOOLEAN
		  | BYTE_NUMBER
		  | DOUBLE
		  | DATE
		  | CHAR
		  | DECIMAL_NUMBER
		  | OBJECT
		  ;
		
UnarExpr: UnarMinus	Expression
		| UnarPlus Expression
		| Not Expression
		;

FunctionDeclaration: Function IDENTIFIER '(' OptEndl ')' EndList BodyStmt
                   | Function IDENTIFIER '(' OptEndl ')' AS Type EndList BodyStmt
                   | Function IDENTIFIER '(' OptEndl IDENTIFIERlist ')' EndList BodyStmt
                   | Function IDENTIFIER '(' OptEndl IDENTIFIERlist ')' AS Type EndList BodyStmt
                   ;
				   
SubDeclaration: Sub IDENTIFIER '('OptEndl')' StatementList END Sub EndList
              | Sub IDENTIFIER '('OptEndl IDENTIFIERlist')' StatementList END Sub EndList
              ;

IfStmt: IF Expression THEN EndList StatementList END IF
	| IF Expression THEN EndList StatementList ELSE EndList StatementList END IF
	| IF Expression THEN EndList TernarOperator END IF
	| IF Expression THEN EndList StatementList ELSEIF Expression THEN EndList StatementList END IF
	;

TernarOperator: Iif '('Expression ',' Expression ',' Expression')';

WhileStatement: WHILE Expression EndList StatementList END WHILE
			| WHILE Expression EndList IF Expression THEN EndList StatementList CONTINUE WHILE EndList END IF EndList StatementList END WHILE
			| WHILE IF Expression THEN Statement EXIT WHILE END IF END WHILE
			;

DoLoopUntilStatement: DO UNTIL Expression EndList StatementList LOOP
					| DO UNTIL Expression EndList StatementList DOOption EndList StatementList LOOP
					;
	
DOOption: EXITDO
		| CONTINUEDO
		;

DoLoopWhileStatement: DO WHILE Expression EndList StatementList LOOP
					| DO WHILE Expression EndList StatementList DOOption EndList StatementList LOOP
					;
	
EXITDO: EXIT DO;

CONTINUEDO: CONTINUE DO;

ForStatement: FOR Statement '=' Statement TO Statement StatementList NEXT
			| FOR Statement '=' Statement TO Statement STEP Statement StatementList NEXT
			| FOR Statement '=' Statement TO Statement StatementList IF Expression THEN EndList CONTINUE FOR EndList END IF EndList StatementList NEXT
			| FOR Statement '=' Statement TO Statement StatementList IF Expression THEN EndList EXIT FOR EndList END IF EndList StatementList NEXT
			;
	
ArrayElementExpression: IDENTIFIER '('')'
                      ;
				 
EndList: TOKEN_LINE
	   | EndList TOKEN_LINE
	   ;
	   
OptEndl: EndList
	   |
	   ;