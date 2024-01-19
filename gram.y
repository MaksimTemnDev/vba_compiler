%{
    #include <iostream>
    #include "classesnodes.h"
    void yyerror(char const* s);
    extern int yylex(void);
    using namespace std;
    CodeNode* global_program_code;
%}

%union {
    int int_literal;
    string* string_literal;
    string* identifier;
    bool bool_literal;
    float float_literal;
    char char_literal;
    object obj_literal;
    double db_literal;

    CodeNode* code;
    ExprNode* expr;
    ExprListNode * expr_list;
    StmtNode* stmt;
    StmtListNode* stmt_list;
    FuncDecl* func_decl;
    SubDecl* sub_decl;
    FuncParamNode* function_param;
    FuncParamListNode* function_params;
    TypeNode* type;
}

%type <code> Program

%type <int_literal> Integer
%type <string_literal> String
%type <identifier> IDENTIFIER

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

%token Integer
%token STRING
%token DECIMAL_NUMBER
%token BYTE_NUMBER
%token DOUBLE
%token ARRAY_ELEMENT_ACCESS_OPERATOR
%token BOOLEAN
%token SINGLE
%token String
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

%start Program 

%%

Program: OptEndl GlobalCodeList { $$ = global_program_code = new CodeNode($2); }
	   ;

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
			  
IDENTIFIEREndl: IDENTIFIER OptEndl {}

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
				 
ArrayElementExpression: IDENTIFIER '('')'{}
					  | IDENTIFIER '('Indexes')'{}
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
			  
Expression: ExprStart '=' OptEndl ExpressionWithoutAssign
		  | ExpressionWithoutAssign
		  ;


ExpressionWithoutAssign: ExprStart '+' OptEndl Expression
		  | ExprStart '&' OptEndl Expression
		  | ExprStart '-' OptEndl Expression
		  | ExprStart '/' OptEndl Expression
		  | ExprStart '*' OptEndl Expression
		  | ExprStart '\\' OptEndl Expression
		  | ExprStart MOD OptEndl Expression
		  | ExprStart '>' OptEndl Expression
		  | ExprStart '<' OptEndl Expression
		  | ExprStart MORE_OR_SAME OptEndl Expression
		  | ExprStart LESS_OR_SAME OptEndl Expression
		  | ExprStart NOT_EQUAL OptEndl Expression
		  | ExprStart BIT_LEFT_SHIFT OptEndl Expression
		  | ExprStart BIT_RIGHT_SHIFT OptEndl Expression
		  | UnarExpr
		  | ArrayStatement
		  | '('Expression')'
		  | TernarOperator
		  ;
	
ExprStart: Values
		 | IDENTIFIER '('ExpressionList')' {}
		 ;
		
Values: SINGLE
	  | String {}
	  | BOOLEAN
	  | DOUBLE
	  | DATE
	  | CHAR
	  | OBJECT
	  | DECIMAL_NUMBER
	  | Indexes
	  ;
	  
Indexes: Integer {}
	   | BYTE_NUMBER
	   | SHORT
	   | IDENTIFIER {}
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
				 
EndList: TOKEN_LINE
	   | EndList TOKEN_LINE
	   ;
	   
OptEndl: EndList
	   |
	   ;
	   
%%