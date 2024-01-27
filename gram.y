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
    double single_literal;
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
	StaticDim* static_;
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
	OptionalStep* optStep;
	
}

%type <code> Program
%type <expr> Expression
%type <expr_list> ExpressionList
%type <expr> ExprStartWithId
%type <expr> ExpressionWithoutAssign
%type <expr> UnarExpr
%type <expr> ArrayExpr
%type <expr> IsNotIs
%type <expr> TernarOperator
%type <expr> ArrayBody

%type <value> IndexesWithId
%type <expr> AssignExprVar
%type <expr> ExprStart
%type <stmt> Statement
%type <stmt> WhileStatement
%type <stmt> DoLoopWhileStatement
%type <stmt> DoLoopUntilStatement
%type <stmt> ForStatement
%type <stmt_list> StatementList
%type <func_decl> FunctionDeclaration
%type <sub_decl> SubDeclaration
%type <type> Type 
%type <globalCodeList> GlobalCodeList
%type <globalCode> GlobalCode
%type <dimStmt> DimStmt
%type <dimStmt> DimSingle
%type <dimStmt> DimArray
%type <static_> StaticStmt
%type <ifNode> IfStmt
%type <idList> IDENTIFIERlist
%type <idList> IDENTIFIEREndl
%type <dimStmt> ArrayIDdeclaration
%type <optStep> OptionalStep
%type <int_literal> Integer
%type <string_literal> STRING
%type <identifier> IDENTIFIER
%type <bool_literal> Boolean
%type <double_literal> DOUBLE
%type <single_literal> SINGLE
%type <char_literal> CHAR
%type <obj_literal> OBJECT
%type <date_literal> DATE
%type <decimal_number> DECIMAL_NUMBER
%type <byte_number> BYTE_NUMBER
%type <short_literal> SHORT
%type <value> Values
%type <value> ValuesWithId
%type <value> Indexes
%type <identificator> ArraySizeName

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

Statement: DimStmt EndList { $$ = StmtNode::DeclarationDim($1, StmtNode::dim_); }
 		 | IfStmt EndList { $$ = StmtNode::DeclarationIf($1, StmtNode::ifstmt_); }
   		 | WhileStatement EndList { $$ = StmtNode::DeclarationWhile($1, StmtNode::while_); }
		 | DoLoopWhileStatement EndList { $$ = StmtNode::DeclarationWhile($1, StmtNode::dowhile_); }
		 | DoLoopUntilStatement EndList { $$ = StmtNode::DeclarationWhile($1, StmtNode::dountil_); }
		 | ForStatement EndList { $$ = StmtNode::DeclarationFor($1, StmtNode::for_); }
		 | StaticStmt EndList { $$ = StmtNode::DeclarationDim($1, StmtNode::static_); }
		 | Expression EndList { $$ = StmtNode::DeclarationExpression($1, StmtNode::expr_); }
		 | ContinueWhile EndList { $$ = StmtNode::DeclarationContinueWhile(StmtNode::continue_while); }
		 | DOOption EndList { $$ = StmtNode::DeclarationDoOption(StmtNode::dooption_exit); }
		 | ContinueExitFor EndList { $$ = StmtNode::DeclarationContinueExitFor(StmtNode::continue_for); }
		 ;

DimStmt: DIM DimSingle {$$ = $2;}
	   | DIM DimArray {$$ = $2;}
	   ;

DimSingle: IDENTIFIERlist '=' Expression { $$ = DimStmt::DeclarationSingleExpr($1, DimStmt::single_expr, $3); }
		 | IDENTIFIERlist AS Type { $$ = DimStmt::DeclarationSingleType($1, DimStmt::single_type, $3); }
		 ;

DimArray: ArrayIDdeclaration { $$ = DimStmt::DeclarationArray($1, DimStmt::array_without, 0); }
	    | ArrayIDdeclaration AS Type { $$ = DimStmt::DeclarationArray($1, DimStmt::array_with, $3); }
		;

IDENTIFIERlist: IDENTIFIEREndl { $$ = $1; }
 			  | IDENTIFIERlist ',' IDENTIFIEREndl { $$ = IdList::IsList($1); }
			  ;
			  
IDENTIFIEREndl: IDENTIFIER OptEndl { $$ = IdList::IsList($1); }

StaticStmt: KW_STATIC DimSingle { $$ = StaticDim::DeclareStatic($2); }
	      | KW_STATIC DimArray { $$ = StaticDim::DeclareStatic($2); }
	      ;

Type: TYPE_BOOLEAN { $$ = TypeNode::TypeNode(TypeNode::bool_); }
	| TYPE_BYTE { $$ = TypeNode::TypeNode(TypeNode::byte_); }
	| TYPE_INTEGER { $$ = TypeNode::TypeNode(TypeNode::int_); }
	| TYPE_SINGLE { $$ = TypeNode::TypeNode(TypeNode::single); }
	| TYPE_SHORT { $$ = TypeNode::TypeNode(TypeNode::short_); }
	| TYPE_DOUBLE { $$ = TypeNode::TypeNode(TypeNode::double_); }
	| TYPE_DECIMAL { $$ = TypeNode::TypeNode(TypeNode::decimal_); }
	| TYPE_DATE { $$ = TypeNode::TypeNode(TypeNode::date_); }
	| TYPE_CHAR { $$ = TypeNode::TypeNode(TypeNode::char_); }
	| TYPE_STRING { $$ = TypeNode::TypeNode(TypeNode::string_); }
	| TYPE_OBJECT { $$ = TypeNode::TypeNode(TypeNode::obj_); }
	;

ArrayBody: IDENTIFIERlist { $$ = $1; }
		 | '{' '}' { $$ = ExprNode::OperatorExpr(ExprNode::arr_empty, 0, 0); }
		 ;

ArrayExpr: '{' ArrayBody '}' { $$ = ExprNode::OperatorExpr(ExprNode::arr_body, $2, 0); }
         | '{' '}' { $$ = ExprNode::OperatorExpr(ExprNode::arr_empty, 0, 0); }
	     | NEW Type '('')' '{'ArrayBody'}' { $$ = ExprNode::OperatorExpr(ExprNode::arr_body_type, $2, $6); }
         ;

ArrayIDdeclaration: ArraySizeName { $$ = $1; }
				  | ArrayIDdeclaration ',' ArraySizeName { $$ = DimStmt::DeclarationArray($1, DimStmt::without, 0); }
				  ;
				 
ArraySizeName: IDENTIFIER '('')' { $$ = Identificator::Identificator($1, Identificator::arr_); }
			 | IDENTIFIER '('IndexesWithId')' {  }
             ;

StatementList: Statement { $$ = $1 }
             | StatementList Statement { $$ = StmtListNode::StmtListNode($2); }
             ;

BodyStmt: StatementList RETURN Expression EndList END Function EndList {}
		| RETURN Expression END Function EndList
		;
		

ExpressionList: Expression { $$ = $1; }
			  | ExpressionList ',' Expression { $$ = ExprListNode::ExprListNode($1); }
			  ;
			  
Expression: AssignExprVar { $$ = $1; }
		  | ExprStart '=' OptEndl ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::no_assign_part, $1, $4); }
		  | ExpressionWithoutAssign { $$ = $1; }
		  | Expression OR Expression { $$ = ExprNode::OperatorExpr(ExprNode::or, $1, $3); }
		  | Expression ORELSE Expression { $$ = ExprNode::OperatorExpr(ExprNode::or_elase, $1, $3); }
		  | Expression AND Expression { $$ = ExprNode::OperatorExpr(ExprNode::and, $1, $3); }
		  | Expression ANDALSO Expression { $$ = ExprNode::OperatorExpr(ExprNode::and_also, $1, $3); }
		  | ExprStartWithId PLUS_ASSIGNMENT ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::plus_assign, $1, $3); }
		  | ExprStartWithId MINUS_ASSIGNMENT ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::minus_assign, $1, $3); }
		  | ExprStartWithId MUL_ASSIGNMENT ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::mul_assign, $1, $3); }
		  | ExprStartWithId DIV_ASSIGNMENT ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::div_assign, $1, $3); }
		  | ExprStartWithId EXP_ASSIGNMENT ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::expr_assign, $1, $3); }
		  | ExprStartWithId BIT_AND_ASSIGNMENT ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::bit_and_aassign, $1, $3); }
		  | ExprStartWithId DIV_NUM_ASSIGNMENT ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::div_num_assign, $1, $3); }
		  | ExprStartWithId BIT_LEFT_SHIFT_ASSIGNMENT ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::bit_l_shift_assign, $1, $3); }
		  | ExprStartWithId BIT_RIGHT_SHIFT_ASSIGNMENT ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::bit_r_shift_assign, $1, $3); }
		  ;
		  
AssignExprVar: IDENTIFIER '=' OptEndl ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::assign, $1, 0); };

ExpressionWithoutAssign: ExprStartWithId '+' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::b_plus, $1, $4); }
		  | ExprStartWithId '&' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::str_plus, $1, $4); }
		  | ExprStartWithId '-' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::b_minus, $1, $4); }
		  | ExprStartWithId '/' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::b_div, $1, $4); }
		  | ExprStartWithId '*' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::b_mul, $1, $4); }
		  | ExprStartWithId '^' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::degree, $1, $4); }
		  | ExprStartWithId '\\' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::int_div, $1, $4); }
		  | ExprStartWithId MOD OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::mod_div, $1, $4); }
		  | ExprStartWithId '>' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::more, $1, $4); }
		  | ExprStartWithId '<' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::less, $1, $4); }
		  | ExprStartWithId MORE_OR_SAME OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::more_s, $1, $4); }
		  | ExprStartWithId LESS_OR_SAME OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::less_s, $1, $4); }
		  | ExprStartWithId NOT_EQUAL OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::not_eq, $1, $4); }
		  | ExprStartWithId BIT_LEFT_SHIFT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::bit_l_shift, $1, $4); }
		  | ExprStartWithId BIT_RIGHT_SHIFT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::bit_r_shift, $1, $4); }
		  | UnarExpr {$$ = $1;}
		  | ArrayExpr {$$ = $1;}
		  | '('Expression')' {$$ = $2;}
		  | TernarOperator {$$ = $1}
		  | IDENTIFIER'('IndexesWithId')' { $$ = ExprNode::OperatorExpr(ExprNode::array_access, $1, $3); }
		  | ExprStartWithId Like ExprStartWithId { $$ = ExprNode::OperatorExpr(ExprNode::like, $1, $3); }
		  | IsNotIs {$$=$1;}
		  | TypeOf IsNotIs { $$ = ExprNode::typeOfisnotIs(ExprNode::typof, $2); }
		  ;


IsNotIs: ExprStartWithId IsNot ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::isnot, $1, $3); }
	   | ExprStartWithId Is ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::is, $1, $3); }
	   ;
	
ExprStart: Values { $$ = $1; }
		 | IDENTIFIER '('ExpressionList')' {}
		 ;
		
Values: SINGLE { $$ = Value::Value($1, Value::single_, FALSE, Identificator::Identificator(single, Identificator::var_)); }
	  | STRING { $$ = Value::Value($1, Value::string_, FALSE, Identificator::Identificator(string, Identificator::var_)); }
	  | Boolean { $$ = $1; }
	  | DOUBLE { $$ = Value::Value($1, Value::double_, FALSE, Identificator::Identificator(double, Identificator::var_)); }
	  | DATE { $$ = Value::Value($1, Value::date_, FALSE, Identificator::Identificator(date, Identificator::var_)); }
	  | CHAR { $$ = Value::Value($1, Value::char_, FALSE, Identificator::Identificator(char, Identificator::var_)); }
	  | OBJECT { $$ = Value::Value($1, Value::obj_, FALSE, Identificator::Identificator(object, Identificator::var_)); }
	  | DECIMAL_NUMBER { $$ = Value::Value($1, Value::dec_num, FALSE, Identificator::Identificator(int, Identificator::var_)); }
	  | Indexes { $$ = $1; }
	  ;
	 
Boolean: KW_FALSE { $$ = Value::Value(0, Value::bool_, FALSE, Identificator::Identificator(FALSE, Identificator::var_)); }
	   | KW_TRUE { $$ = Value::Value(1, Value::bool_, TRUE, Identificator::Identificator(TRUE, Identificator::var_)); }
	   ;
	 
Indexes: Integer { $$ = Value::Value($1, Value::int_, TRUE, Identificator::Identificator(int, Identificator::var_)); } //что передавать первым параметром
	   | BYTE_NUMBER { $$ = Value::Value($1, Value::byte_num, TRUE, Identificator::Identificator(byte, Identificator::var_)); }
	   | SHORT { $$ = Value::Value($1, Value::short_num, TRUE, Identificator::Identificator(short, Identificator::var_)); }
	   ;
	   
IndexesWithId: Indexes { $$ = $1; }
			 | IDENTIFIER { $$ = Identificator::Identificator($1, Identificator::var_); }
			 ;
			 
ExprStartWithId: ValuesWithId {}
			   | IDENTIFIER '('ExpressionList')' {}
			   ;
		
ValuesWithId: SINGLE { $$ = Value::Value($1, Value::single_, FALSE, Identificator::Identificator(single, Identificator::var_)); }
            | STRING { $$ = Value::Value($1, Value::string_, FALSE, Identificator::Identificator(string, Identificator::var_)); }
            | Boolean { $$ = $1; }
            | DOUBLE { $$ = Value::Value($1, Value::double_, FALSE, Identificator::Identificator(double, Identificator::var_)); }
            | DATE { $$ = Value::Value($1, Value::date_, FALSE, Identificator::Identificator(date, Identificator::var_)); }
            | CHAR { $$ = Value::Value($1, Value::char_, FALSE, Identificator::Identificator(char, Identificator::var_)); }
            | OBJECT { $$ = Value::Value($1, Value::obj_, FALSE, Identificator::Identificator(object, Identificator::var_)); }
            | DECIMAL_NUMBER { $$ = Value::Value($1, Value::dec_num, FALSE, Identificator::Identificator(int, Identificator::var_)); }
		    | IndexesWithId { $$ = $1; }
		    ;

UnarExpr: UnarMinus	ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::u_minus, 0, $2); }
		| UnarPlus ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::u_plus, 0, $2); }
		| Not Expression { $$ = ExprNode::OperatorExpr(ExprNode::not, 0, $2); }
		;

FunctionDeclaration: Function IDENTIFIER '(' OptEndl ')' EndList BodyStmt {}
                   | Function IDENTIFIER '(' OptEndl ')' AS Type EndList BodyStmt {}
                   | Function IDENTIFIER '(' OptEndl IDENTIFIERlist ')' EndList BodyStmt {}
                   | Function IDENTIFIER '(' OptEndl IDENTIFIERlist ')' AS Type EndList BodyStmt {}
                   ;
				   
SubDeclaration: Sub IDENTIFIER '('OptEndl')' StatementList END Sub EndList {}
              | Sub IDENTIFIER '('OptEndl IDENTIFIERlist')' StatementList END Sub EndList {}
              ;

IfStmt: IF Expression THEN EndList StatementList END IF { $$ = IfNode::IfClear($2, $5, IfNode::clear_); }
	| IF Expression THEN EndList StatementList ELSE EndList StatementList END IF { $$ = IfNode::IfElse($2, $5, $8, IfNode::else_); }
	| IF Expression THEN EndList TernarOperator END IF { $$ = IfNode::IfTernar($2, $5, IfNode::ternar_); }
	| IF Expression THEN EndList StatementList ELSEIF Expression THEN EndList StatementList END IF { $$ = IfNode::IfElseIf($2, $5, $7, $10, IfNode::else_); }
	;

TernarOperator: Iif '('Expression ',' Expression ',' Expression')' { $$ = Ternar::ternarOp($3, $5, $7); };

WhileStatement: WHILE Expression EndList StatementList END WHILE { $$ = While::whileStmt($2, $4, While::simple_); };
			
ContinueWhile: CONTINUE WHILE;

DoLoopUntilStatement: DO UNTIL Expression EndList StatementList LOOP { $$ = While::whileStmt($3, $5, While::doloopuntil); };
	
DOOption: EXITDO
		| CONTINUEDO
		;

DoLoopWhileStatement: DO WHILE Expression EndList StatementList LOOP { $$ = While::whileStmt($3, $5, While::doloopwhile_); };
	
EXITDO: EXIT DO;

CONTINUEDO: CONTINUE DO;

OptionalStep: { $$ = OptionalStep::whileStmt(0, false); }
			| STEP IndexesWithId { $$ = OptionalStep::whileStmt($2, true); }
			;
			
ContinueExitFor: CONTINUE FOR
			   | EXIT FOR
			   ;

ForStatement: FOR AssignExprVar TO Expression OptionalStep EndList StatementList NEXT { $$ = ForNode::fornode($2, $4, $5, $7, While::doloopwhile_); }
			;
				 
EndList: TOKEN_LINE
	   | EndList TOKEN_LINE
	   ;
	   
OptEndl: EndList
	   |
	   ;
	   
%%