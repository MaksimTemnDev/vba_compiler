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
    double double_literal;
    char char_literal;
    object obj_literal;
	date date_literal; 
	int decimal_number;
	int short_literal;
	byte byte_number;

    CodeNode* code;
    ExprNode* expr;
    ExprListNode* expr_list;
    StmtNode* stmt;
    StmtListNode* stmt_list;
    FuncDecl* func_decl;
    FuncParamNode* function_param;
    FuncParamListNode* function_params;
    TypeNode* type;
	GlobalCodeList* globalCodeList;
	GlobalCode* globalCode;
	DimStmt* dimStmt;
	Value* value;
	ForNode* forNode;
	ArrayIdList* arrayIdList;
	ArrayIdDeclare* arrayIdDeclare;
	Identificator* identificator;
	IdList* idList;
	While* whileSt;
	Ternar* ternar;
	IfNode* ifNode;
	
}

%type <code> Program
%type <expr> Expression
%type <expr_list> ExpressionList
%type <stmt> Statement
%type <stmt_list> StatementList
%type <func_decl> FunctionDeclaration
%type <sub_decl> SubDeclaration
%type <type> Type 
%type <globalCodeList> GlobalCodeList
%type <globalCode> GlobalCode
%type <dimStmt> DimStmt

%type <int_literal> Integer
%type <string_literal> STRING
%type <identifier> IDENTIFIER
%type <bool_literal> Boolean
%type <double_literal> DOUBLE
%type <char_literal> CHAR
%type <obj_literal> OBJECT
%type <date_literal> DATE
%type <decimal_number> DECIMAL_NUMBER
%type <byte_number> BYTE_NUMBER
%type <short_literal> SHORT 

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
%token KW_FALSE
%token KW_TRUE


%token Integer
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

%token TRUE
%token FALSE
%token PLUS_ASSIGNMENT MINUS_ASSIGNMENT MUL_ASSIGNMENT DIV_ASSIGNMENT EXP_ASSIGNMENT BIT_AND_ASSIGNMENT DIV_NUM_ASSIGNMENT BIT_LEFT_SHIFT_ASSIGNMENT BIT_RIGHT_SHIFT_ASSIGNMENT

%left '^'
%left UnarPlus UnarMinus
%right '*' '/'
%right '\\'
%right MOD
%right '+' '-'
%right '&'
%left BIT_LEFT_SHIFT BIT_RIGHT_SHIFT
%right '=' NOT_EQUAL '<' LESS_OR_SAME Is IsNot Like TypeOf Not PLUS_ASSIGNMENT MINUS_ASSIGNMENT MUL_ASSIGNMENT DIV_ASSIGNMENT EXP_ASSIGNMENT BIT_AND_ASSIGNMENT DIV_NUM_ASSIGNMENT BIT_LEFT_SHIFT_ASSIGNMENT BIT_RIGHT_SHIFT_ASSIGNMENT
%left '>' MORE_OR_SAME
%left OR ORELSE
%left AND ANDALSO
%nonassoc '{' '}'

%start Program 

%%

Program: OptEndl GlobalCodeList { $$ = global_program_code = new CodeNode($2); }
	   ;

GlobalCodeList: GlobalCode {}
			  | GlobalCodeList GlobalCode {}
			  ;

GlobalCode: FunctionDeclaration {}
		  | SubDeclaration {}
		  | DimStmt EndList {}
		  ;

Statement: DimStmt EndList {}
 		 | IfStmt EndList {}
   		 | WhileStatement EndList {}
		 | DoLoopWhileStatement EndList {}
		 | DoLoopUntilStatement EndList {}
		 | ForStatement EndList {}
		 | StaticStmt EndList {}
		 | Expression EndList {}
		 | ContinueWhile EndList {}
		 | DOOption EndList {}
		 | ContinueExitFor EndList {}
		 ;

DimStmt: DIM DimSingle {}
	   | DIM DimArray {}
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

Type: TYPE_BOOLEAN {}
	| TYPE_BYTE {}
	| TYPE_INTEGER {}
	| TYPE_SINGLE {}
	| TYPE_SHORT {}
	| TYPE_DOUBLE {}
	| TYPE_DECIMAL {}
	| TYPE_DATE {}
	| TYPE_CHAR {}
	| TYPE_STRING {}
	| TYPE_OBJECT {}
	;

ArrayBody: IDENTIFIERlist
		 | '{' '}'
		 ;

ArrayExpr: '{' ArrayBody '}'
         | '{' '}'
	     | NEW Type '('')' '{'ArrayBody'}'
         ;

ArrayIDdeclaration: ArraySizeName
				  | ArrayIDdeclaration ',' ArraySizeName
				  ;
				 
ArraySizeName: IDENTIFIER '('')'{}
			 | IDENTIFIER '('IndexesWithId')'{}
             ;

StatementList: Statement {}
             | StatementList Statement {}
             ;

BodyStmt: StatementList RETURN Expression EndList END Function EndList {}
		| RETURN Expression END Function EndList
		;
		

ExpressionList: Expression {}
			  | ExpressionList ',' Expression {}
			  ;
			  
Expression: AssignExprVar {}
		  | ExprStart '=' OptEndl ExpressionWithoutAssign {};
		  | ExpressionWithoutAssign {}
		  | Expression OR Expression {}
		  | Expression ORELSE Expression {}
		  | Expression AND Expression {}
		  | Expression ANDALSO Expression {}
		  | ExprStartWithId PLUS_ASSIGNMENT ExpressionWithoutAssign {}
		  | ExprStartWithId MINUS_ASSIGNMENT ExpressionWithoutAssign {}
		  | ExprStartWithId MUL_ASSIGNMENT ExpressionWithoutAssign {}
		  | ExprStartWithId DIV_ASSIGNMENT ExpressionWithoutAssign {}
		  | ExprStartWithId EXP_ASSIGNMENT ExpressionWithoutAssign {}
		  | ExprStartWithId BIT_AND_ASSIGNMENT ExpressionWithoutAssign {}
		  | ExprStartWithId DIV_NUM_ASSIGNMENT ExpressionWithoutAssign {}
		  | ExprStartWithId BIT_LEFT_SHIFT_ASSIGNMENT ExpressionWithoutAssign {}
		  | ExprStartWithId BIT_RIGHT_SHIFT_ASSIGNMENT ExpressionWithoutAssign {}
		  ;
		  
AssignExprVar: IDENTIFIER '=' OptEndl ExpressionWithoutAssign {};

ExpressionWithoutAssign: ExprStartWithId '+' OptEndl Expression
		  | ExprStartWithId '&' OptEndl Expression
		  | ExprStartWithId '-' OptEndl Expression
		  | ExprStartWithId '/' OptEndl Expression
		  | ExprStartWithId '*' OptEndl Expression
		  | ExprStartWithId '^' OptEndl Expression
		  | ExprStartWithId '\\' OptEndl Expression
		  | ExprStartWithId MOD OptEndl Expression
		  | ExprStartWithId '>' OptEndl Expression
		  | ExprStartWithId '<' OptEndl Expression
		  | ExprStartWithId MORE_OR_SAME OptEndl Expression
		  | ExprStartWithId LESS_OR_SAME OptEndl Expression
		  | ExprStartWithId NOT_EQUAL OptEndl Expression
		  | ExprStartWithId BIT_LEFT_SHIFT OptEndl Expression
		  | ExprStartWithId BIT_RIGHT_SHIFT OptEndl Expression
		  | UnarExpr
		  | ArrayExpr
		  | '('Expression')'
		  | TernarOperator
		  | IDENTIFIER'('IndexesWithId')' {}
		  | ExprStartWithId Like ExprStartWithId
		  | IsNotIs
		  | TypeOf IsNotIs
		  ;
		  
IsNotIs: ExprStartWithId IsNot ExpressionWithoutAssign
	   | ExprStartWithId Is ExpressionWithoutAssign
	   ;
	
ExprStart: Values
		 | IDENTIFIER '('ExpressionList')' {}
		 ;
		
Values: SINGLE
	  | STRING {}
	  | Boolean {}
	  | DOUBLE {}
	  | DATE {}
	  | CHAR {}
	  | OBJECT {}
	  | DECIMAL_NUMBER {}
	  | Indexes
	  ;
	 
Boolean: KW_FALSE {}
	   | KW_TRUE {}
	   ;
	 
Indexes: Integer {}
	   | BYTE_NUMBER {}
	   | SHORT {}
	   ;
	   
IndexesWithId: Indexes
			 | IDENTIFIER {}
			 ;
			 
ExprStartWithId: ValuesWithId
			   | IDENTIFIER '('ExpressionList')' {}
			   ;
		
ValuesWithId: SINGLE
		    | STRING {}
		    | Boolean {}
		    | DOUBLE {}
		    | DATE {}
		    | CHAR {}
		    | OBJECT {}
		    | DECIMAL_NUMBER {}
		    | IndexesWithId
		    ;

UnarExpr: UnarMinus	ExpressionWithoutAssign
		| UnarPlus ExpressionWithoutAssign
		| Not Expression
		;

FunctionDeclaration: Function IDENTIFIER '(' OptEndl ')' EndList BodyStmt {}
                   | Function IDENTIFIER '(' OptEndl ')' AS Type EndList BodyStmt {}
                   | Function IDENTIFIER '(' OptEndl IDENTIFIERlist ')' EndList BodyStmt {}
                   | Function IDENTIFIER '(' OptEndl IDENTIFIERlist ')' AS Type EndList BodyStmt {}
                   ;
				   
SubDeclaration: Sub IDENTIFIER '('OptEndl')' StatementList END Sub EndList {}
              | Sub IDENTIFIER '('OptEndl IDENTIFIERlist')' StatementList END Sub EndList {}
              ;

IfStmt: IF Expression THEN EndList StatementList END IF
	| IF Expression THEN EndList StatementList ELSE EndList StatementList END IF
	| IF Expression THEN EndList TernarOperator END IF
	| IF Expression THEN EndList StatementList ELSEIF Expression THEN EndList StatementList END IF
	;

TernarOperator: Iif '('Expression ',' Expression ',' Expression')';

WhileStatement: WHILE Expression EndList StatementList END WHILE;
			
ContinueWhile: CONTINUE WHILE;

DoLoopUntilStatement: DO UNTIL Expression EndList StatementList LOOP;
	
DOOption: EXITDO
		| CONTINUEDO
		;

DoLoopWhileStatement: DO WHILE Expression EndList StatementList LOOP;
	
EXITDO: EXIT DO;

CONTINUEDO: CONTINUE DO;

OptionalStep: 
			| STEP IndexesWithId
			;
			
ContinueExitFor: CONTINUE FOR
			   | EXIT FOR
			   ;

ForStatement: FOR AssignExprVar TO Expression OptionalStep EndList StatementList NEXT
			;
				 
EndList: TOKEN_LINE
	   | EndList TOKEN_LINE
	   ;
	   
OptEndl: EndList
	   |
	   ;
	   
%%