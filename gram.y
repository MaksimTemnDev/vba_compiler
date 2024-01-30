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
	int decimal_number;

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
	BodyStmt* bodyStmt;
}

%type <code> Program
%type <expr> Expression
%type <expr_list> ExpressionList
%type <expr> ExprStartWithId
%type <expr> ExpressionWithoutAssign
%type <expr> UnarExpr
%type <expr> ArrayExpr
%type <expr> ArrayBody
%type <expr> IsNotIs
%type <ternar> TernarOperator

%type <value> IndexesWithId
%type <expr> AssignExprVar
%type <expr> ExprStart
%type <stmt> Statement
%type <whileSt> WhileStatement
%type <whileSt> DoLoopWhileStatement
%type <whileSt> DoLoopUntilStatement
%type <forNode> ForStatement
%type <stmt> ContinueExitFor
%type <stmt> DOOption
%type <stmt_list> StatementList
%type <func_decl> FunctionDeclaration
%type <bodyStmt> BodyStmt
%type <func_decl> SubDeclaration
%type <type> Type 
%type <globalCodeList> GlobalCodeList
%type <globalCode> GlobalCode
%type <dimStmt> DimStmt
%type <dimStmt> DimSingle
%type <dimStmt> DimArray
%type <dimStmt> StaticStmt
%type <ifNode> IfStmt
%type <idList> IDENTIFIERlist
%type <identificator> IDENTIFIEREndl
%type <arrayIdList> ArrayIDdeclaration
%type <optStep> OptionalStep
%type <string_literal> STRING
%type <identificator> IDENTIFIER
%type <value> Boolean
%type <double_literal> DOUBLE
%type <char_literal> CHAR
%type <decimal_number> DECIMAL_NUMBER
%type <value> Values
%type <value> ValuesWithId
%type <value> Indexes
%type <identificator> ArraySizeName

%token END
%token WHILE DO LOOP UNTIL FOR TO STEP CONTINUE EXIT
%token IF THEN ELSE ELSEIF
%token DIM
%token NEW AS RETURN NEXT
%token IDENTIFIER
%token <type> TYPE_BOOLEAN
%token <type> TYPE_BYTE
%token <type> TYPE_INTEGER
%token <type> TYPE_SINGLE
%token <type> TYPE_SHORT
%token <type> TYPE_DOUBLE
%token <type> TYPE_DECIMAL
%token <type> TYPE_DATE
%token <type> TYPE_CHAR
%token <type> TYPE_STRING
%token <type> TYPE_OBJECT
%token TOKEN_LINE
%token Function
%token Sub
%token Iif
%token KW_STATIC
%token <bool_literal> KW_FALSE
%token <bool_literal> KW_TRUE


%token STRING
%token DECIMAL_NUMBER
%token DOUBLE
%token CHAR

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

GlobalCodeList: GlobalCode { $$ = new GlobalCodeList($1); }
			  | GlobalCodeList GlobalCode { $$ = GlobalCodeList::Append($1, $2); }
			  ;

GlobalCode: FunctionDeclaration { $$ = GlobalCode::addSubFunc($1); }
		  | SubDeclaration { $$ = GlobalCode::addSubFunc($1); }
		  | DimStmt EndList { $$ = GlobalCode::addDim($1); }
		  ;

Statement: DimStmt EndList { $$ = StmtNode::DeclarationDim($1, StmtNode::dim_, 0); }
 		 | IfStmt EndList { $$ = StmtNode::DeclarationIf($1, StmtNode::ifstmt_); }
   		 | WhileStatement EndList { $$ = StmtNode::DeclarationWhile($1, StmtNode::while_); }
		 | DoLoopWhileStatement EndList { $$ = StmtNode::DeclarationWhile($1, StmtNode::dowhile_); }
		 | DoLoopUntilStatement EndList { $$ = StmtNode::DeclarationWhile($1, StmtNode::dountil_); }
		 | ForStatement EndList { $$ = StmtNode::DeclarationFor($1, StmtNode::for_); }
		 | StaticStmt EndList { $$ = StmtNode::DeclarationDim($1, StmtNode::static_, 1); }
		 | Expression EndList { $$ = StmtNode::DeclarationExpression($1, StmtNode::expr_); }
		 | ContinueWhile EndList { $$ = StmtNode::DeclarationContinueWhile(StmtNode::continue_while); }
		 | DOOption EndList { $$ = $1; }
		 | ContinueExitFor EndList { $$ = $1; }
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

IDENTIFIERlist: IDENTIFIEREndl { $$ = new IdList($1); }
 			  | IDENTIFIERlist ',' IDENTIFIEREndl { $$ = IdList::Append($1, $3); }
			  ;
			  
IDENTIFIEREndl: IDENTIFIER OptEndl { $$ = $1; }

StaticStmt: KW_STATIC DimSingle { $$ = new DimStmt($2); }
	      | KW_STATIC DimArray { $$ = new DimStmt($2); }
	      ;

Type: TYPE_BOOLEAN { $$ = new TypeNode($1); }
	| TYPE_BYTE { $$ = new TypeNode($1); }
	| TYPE_INTEGER { $$ = new TypeNode($1); }
	| TYPE_SINGLE { $$ = new TypeNode($1); }
	| TYPE_SHORT { $$ = new TypeNode($1); }
	| TYPE_DOUBLE { $$ = new TypeNode($1); }
	| TYPE_DECIMAL { $$ = new TypeNode($1); }
	| TYPE_DATE { $$ = new TypeNode($1); }
	| TYPE_CHAR { $$ = new TypeNode($1); }
	| TYPE_STRING { $$ = new TypeNode($1); }
	| TYPE_OBJECT { $$ = new TypeNode($1); }
	;

ArrayBody: '{' IDENTIFIERlist '}' { $$ = ExprNode::arrayBodyIdList($2, ExprNode::arr_body); }
		 | '{' ExpressionList '}' { $$ = ExprNode::arrayBodyExprList($2, ExprNode::arr_expr_list); }
		 ;

ArrayExpr: ArrayBody { $$ = ExprNode::OperatorExpr(ExprNode::arr_body, $1, 0); }
         | '{' '}' { $$ = ExprNode::OperatorExpr(ExprNode::arr_empty, 0, 0); }
	     | NEW Type '('')' ArrayBody { $$ = new ExprNode($5, ExprNode::arr_body_type, $2); }
         ;

ArrayIDdeclaration: ArraySizeName { $$ = new ArrayIdList($1); }
				  | ArrayIDdeclaration ',' ArraySizeName { $$ = ArrayIdList::Append($1, $3); }
				  ;
				 
ArraySizeName: IDENTIFIER '('')' { $$ = Identificator::id_witout($1, Identificator::arr_); }
			 | IDENTIFIER '('IndexesWithId')' { $$ = Identificator::id_with($1, Identificator::arr_, $3); }
             ;

StatementList: Statement { $$ = new StmtListNode($1); }
             | StatementList Statement { $$ = StmtListNode::Append($1, $2); }
             ;

BodyStmt: StatementList RETURN Expression EndList END Function EndList { $$ = new BodyStmt($3, $1); }
		| RETURN Expression END Function EndList { $$ = new BodyStmt($2, 0); }
		;
		

ExpressionList: Expression { $$ = new ExprListNode($1); }
			  | ExpressionList ',' Expression { $$ = ExprListNode::Append($1, $3); }
			  ;
			  
Expression: AssignExprVar { $$ = $1; }
		  | ExprStart '=' OptEndl ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::no_assign_part, $1, $4); }
		  | ExpressionWithoutAssign { $$ = $1; }
		  | Expression OR Expression { $$ = ExprNode::OperatorExpr(ExprNode::or_, $1, $3); }
		  | Expression ORELSE Expression { $$ = ExprNode::OperatorExpr(ExprNode::or_elase, $1, $3); }
		  | Expression AND Expression { $$ = ExprNode::OperatorExpr(ExprNode::and_, $1, $3); }
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
		  
AssignExprVar: IDENTIFIER '=' OptEndl ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::assign, 0, $4); };

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
		  | ExprStartWithId NOT_EQUAL OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::_not_eq, $1, $4); }
		  | ExprStartWithId BIT_LEFT_SHIFT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::bit_l_shift, $1, $4); }
		  | ExprStartWithId BIT_RIGHT_SHIFT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::bit_r_shift, $1, $4); }
		  | UnarExpr {$$ = $1;}
		  | ArrayExpr {$$ = $1;}
		  | '('Expression')' {$$ = $2;}
		  | TernarOperator {$$ = ExprNode::ternarOp($1, ExprNode::ternar);}
		  | IDENTIFIER'('IndexesWithId')' { $$ = ExprNode::valueExpr(ExprNode::array_access, $1, $3); }
		  | ExprStartWithId Like ExprStartWithId { $$ = ExprNode::OperatorExpr(ExprNode::like, $1, $3); }
		  | IsNotIs {$$=$1;}
		  | TypeOf IsNotIs { $$ = ExprNode::typeOfisnotIs(ExprNode::typof, $2); }
		  ;


IsNotIs: ExprStartWithId IsNot ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::isnot, $1, $3); }
	   | ExprStartWithId Is ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::is, $1, $3); }
	   ;
	
ExprStart: Values { $$ = new ExprNode($1, ExprNode::value); }
		 | IDENTIFIER '('ExpressionList')' { $$ = ExprNode::exprList(ExprNode::expr_start_func, $1, $3); }
		 ;
		
Values: STRING { $$ = new Value($1, Value::string_); }
	  | Boolean { $$ = $1; }
	  | DOUBLE { $$ = new Value($1, Value::Double_); }
	  | CHAR { $$ = new Value($1, Value::Char_); }
	  | Indexes { $$ = $1; }
	  ;
	 
Boolean: KW_FALSE { $$ = new Value($1, Value::bool_); }
	   | KW_TRUE { $$ = new Value($1, Value::bool_); }
	   ;
	 
Indexes: DECIMAL_NUMBER { $$ = new Value($1, Value::dec_num, 1, 0); };
	   
IndexesWithId: Indexes { $$ = $1; }
			 | IDENTIFIER { $$ = new Value($1, Value::id_); }
			 ;
			 
ExprStartWithId: ValuesWithId {$$ = new ExprNode($1, ExprNode::values_with_id);}
			   | IDENTIFIER '('ExpressionList')' { $$ = ExprNode::exprList(ExprNode::expr_start_id, $1, $3); }
			   ;
		
ValuesWithId: STRING { $$ = new Value($1, Value::string_); }
			| Boolean { $$ = $1; }
			| DOUBLE { $$ = new Value($1, Value::Double_); }
			| CHAR { $$ = new Value($1, Value::Char_); }
		    | IndexesWithId { $$ = $1; }
		    ;

UnarExpr: UnarMinus	ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::u_minus, 0, $2); }
		| UnarPlus ExpressionWithoutAssign { $$ = ExprNode::OperatorExpr(ExprNode::u_plus, 0, $2); }
		| Not Expression { $$ = ExprNode::OperatorExpr(ExprNode::not_, 0, $2); }
		;

FunctionDeclaration: Function IDENTIFIER '(' OptEndl ')' EndList BodyStmt { $$ = FuncDecl::funcDeclare($2, 0, 0, $7, 0, 0); }
                   | Function IDENTIFIER '(' OptEndl ')' AS Type EndList BodyStmt { $$ = FuncDecl::funcDeclare($2, $7, 0, $9, 0, 0); }
                   | Function IDENTIFIER '(' OptEndl IDENTIFIERlist ')' EndList BodyStmt { $$ = FuncDecl::funcDeclare($2, 0, $5, $8, 0, 0); }
                   | Function IDENTIFIER '(' OptEndl IDENTIFIERlist ')' AS Type EndList BodyStmt { $$ = FuncDecl::funcDeclare($2, $8, $5, $10, 0, 0); }
                   ;
				   
SubDeclaration: Sub IDENTIFIER '('OptEndl')' EndList StatementList END Sub EndList { $$ = FuncDecl::funcDeclare($2, 0, 0, 0, 1, $7); }
              | Sub IDENTIFIER '('OptEndl IDENTIFIERlist')' EndList StatementList END Sub EndList { $$ = FuncDecl::funcDeclare($2, 0, $5, 0, 1, $8); }
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
	
DOOption: EXITDO { $$ = StmtNode::DeclarationDoOption(StmtNode::dooption_exit); }
		| CONTINUEDO { $$ = StmtNode::DeclarationDoOption(StmtNode::dooption_continue); }
		;

DoLoopWhileStatement: DO WHILE Expression EndList StatementList LOOP { $$ = While::whileStmt($3, $5, While::doloopwhile_); };
	
EXITDO: EXIT DO;

CONTINUEDO: CONTINUE DO;

OptionalStep: { $$ = OptionalStep::addStep(0, false); }
			| STEP IndexesWithId { $$ = OptionalStep::addStep($2, true); }
			;
			
ContinueExitFor: CONTINUE FOR { $$ = StmtNode::DeclarationContinueExitFor(StmtNode::continue_for); }
			   | EXIT FOR { $$ = StmtNode::DeclarationContinueExitFor(StmtNode::exit_for); }
			   ;

ForStatement: FOR AssignExprVar TO Expression OptionalStep EndList StatementList NEXT { $$ = ForNode::fornode($2, $4, $5, 0, $7); }
			;
				 
EndList: TOKEN_LINE
	   | EndList TOKEN_LINE
	   ;
	   
OptEndl: EndList
	   |
	   ;
	   
%%