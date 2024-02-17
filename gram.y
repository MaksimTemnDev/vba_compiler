%{
    #include <iostream>
    #include "classesnodes.h"
    void yyerror(char const* s);
    extern int yylex(void);
    using namespace std;
    CodeNode* global_program_code;
%}

%union {
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
%type <expr> ArrayBody
%type <expr> CallArrOrFunc

%type <stmt> Statement
%type <whileSt> WhileStatement
%type <whileSt> DoLoopWhileStatement
%type <whileSt> DoLoopUntilStatement
%type <forNode> ForStatement
%type <stmt> ContinueExitFor
%type <stmt> DOOption
%type <stmt_list> StatementList
%type <func_decl> FunctionDeclaration
%type <func_decl> SubDeclaration
%type <stmt_list> OptStmtList

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
%type <value> Boolean
%type <double_literal> DOUBLE
%type <char_literal> CHAR
%type <decimal_number> DECIMAL_NUMBER
%type <value> Values
%type <function_params> FuncParamList
%type <function_param> FuncParam

%token END
%token WHILE DO LOOP UNTIL FOR TO STEP CONTINUE EXIT
%token IF THEN ELSE ELSEIF
%token DIM
%token ByRef ByVal
%token NEW RETURN NEXT
%token As
%token <identifier> IDENTIFIER
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
		 | RETURN Expression EndList { $$ = StmtNode::DeclarationReturn($2, StmtNode::return_stmt); }
		 ;

DimStmt: DIM DimSingle {$$ = $2;}
	   | DIM DimArray {$$ = $2;}
	   ;

DimSingle: IDENTIFIERlist '=' Expression { $$ = DimStmt::DeclarationSingleExpr($1, DimStmt::single_expr, $3); }
		 | IDENTIFIERlist As Type '=' Expression { $$ = DimStmt::DeclarationSingleType($1, DimStmt::single_type, $3); }
		 ;

DimArray: ArrayIDdeclaration '=' Expression { $$ = DimStmt::DeclarationArray($1, DimStmt::array_without, 0); }
	    | ArrayIDdeclaration As Type '=' Expression { $$ = DimStmt::DeclarationArray($1, DimStmt::array_with, $3); }
		;

IDENTIFIERlist: IDENTIFIEREndl { $$ = new IdList($1); }
 			  | IDENTIFIERlist ',' IDENTIFIEREndl { $$ = IdList::Append($1, $3); }
			  ;
			  
IDENTIFIEREndl: IDENTIFIER OptEndl { $$ = new Identificator($1); }

StaticStmt: KW_STATIC DimSingle { $$ = new DimStmt($2); }
	      | KW_STATIC DimArray { $$ = new DimStmt($2); }
	      ;

Type: TYPE_BOOLEAN { $$ = new TypeNode(TypeNode::bool_); }
	| TYPE_BYTE { $$ = new TypeNode(TypeNode::byte_); }
	| TYPE_INTEGER { $$ = new TypeNode(TypeNode::int_); }
	| TYPE_SINGLE { $$ = new TypeNode(TypeNode::single); }
	| TYPE_SHORT { $$ = new TypeNode(TypeNode::short_); }
	| TYPE_DOUBLE { $$ = new TypeNode(TypeNode::double_); }
	| TYPE_DECIMAL { $$ = new TypeNode(TypeNode::decimal_); }
	| TYPE_DATE { $$ = new TypeNode(TypeNode::date_); }
	| TYPE_CHAR { $$ = new TypeNode(TypeNode::char_); }
	| TYPE_STRING { $$ = new TypeNode(TypeNode::string_); }
	| TYPE_OBJECT { $$ = new TypeNode(TypeNode::obj_); }
	;

ArrayBody: '{' OptEndl ExpressionList OptEndl'}' { $$ = ExprNode::arrayBodyExprList($3, ExprNode::arr_expr_list); }
		 ;

ArrayIDdeclaration: CallArrOrFunc { $$ = new ArrayIdList($1); }
				  | ArrayIDdeclaration ',' CallArrOrFunc { $$ = ArrayIdList::Append($1, $3); }
				  ;

StatementList: Statement { $$ = new StmtListNode($1); }
             | StatementList Statement { $$ = StmtListNode::Append($1, $2); }
             ;
			 
OptStmtList: { }
		   | StatementList
		   ;
		
ExpressionList: Expression { $$ = new ExprListNode($1); }
			  | ExpressionList ',' OptEndl Expression { $$ = ExprListNode::Append($1, $4); }
			  ;
	
Expression: Expression '=' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::no_assign_part, $1, $4); }
		  | Expression '+' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::b_plus, $1, $4); }
		  | Expression '&' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::str_plus, $1, $4); }
		  | Expression '-' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::b_minus, $1, $4); }
		  | Expression '/' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::b_div, $1, $4); }
		  | Expression '*' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::b_mul, $1, $4); }
		  | Expression '^' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::degree, $1, $4); }
		  | Expression '\\' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::int_div, $1, $4); }
		  | Expression MOD OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::mod_div, $1, $4); }
		  | Expression '>' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::more, $1, $4); }
		  | Expression '<' OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::less, $1, $4); }
		  | Expression MORE_OR_SAME OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::more_s, $1, $4); }
		  | Expression LESS_OR_SAME OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::less_s, $1, $4); }
		  | Expression NOT_EQUAL OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::_not_eq, $1, $4); }
		  | Expression BIT_LEFT_SHIFT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::bit_l_shift, $1, $4); }
		  | Expression BIT_RIGHT_SHIFT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::bit_r_shift, $1, $4); }
		  | '-' Expression %prec UnarMinus { $$ = ExprNode::OperatorExpr(ExprNode::u_minus, 0, $2); }
		  | '+' Expression %prec UnarPlus { $$ = ExprNode::OperatorExpr(ExprNode::u_plus, 0, $2); }
		  | Not Expression { $$ = ExprNode::OperatorExpr(ExprNode::not_, 0, $2); }
		  | ArrayBody { $$ = ExprNode::OperatorExpr(ExprNode::arr_body, $1, 0); }
          | '{'OptEndl'}' { $$ = ExprNode::OperatorExpr(ExprNode::arr_empty, 0, 0); }
	      | NEW Type '('OptEndl')' ArrayBody { $$ = ExprNode::arrayBodyExpr($2, $6, ExprNode::arr_body); }
		  | '('OptEndl Expression OptEndl')' { $$ = ExprNode::OperatorExpr(ExprNode::expr, $3, 0); }
		  | Expression Like OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::like, $1, $4); }
		  | Expression IsNot OptEndl Type { $$ = ExprNode::typeOfisnotIs(ExprNode::isnot, $1); }
		  | Expression Is OptEndl Type { $$ = ExprNode::typeOfisnotIs(ExprNode::is, $1); }
		  | TypeOf Expression { $$ = ExprNode::typeOfisnotIs(ExprNode::typof, $2); }
		  | Expression OR OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::or_, $1, $4); }
		  | Expression ORELSE OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::or_else, $1, $4); }
		  | Expression AND OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::and_, $1, $4); }
		  | Expression ANDALSO OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::and_also, $1, $4); }
		  | Expression PLUS_ASSIGNMENT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::plus_assign, $1, $4); }
		  | Expression MINUS_ASSIGNMENT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::minus_assign, $1, $4); }
		  | Expression MUL_ASSIGNMENT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::mul_assign, $1, $4); }
		  | Expression DIV_ASSIGNMENT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::div_assign, $1, $4); }
		  | Expression EXP_ASSIGNMENT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::expr_assign, $1, $4); }
		  | Expression BIT_AND_ASSIGNMENT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::bit_and_assign, $1, $4); }
		  | Expression DIV_NUM_ASSIGNMENT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::div_num_assign, $1, $4); }
		  | Expression BIT_LEFT_SHIFT_ASSIGNMENT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::bit_l_shift_assign, $1, $4); }
		  | Expression BIT_RIGHT_SHIFT_ASSIGNMENT OptEndl Expression { $$ = ExprNode::OperatorExpr(ExprNode::bit_r_shift_assign, $1, $4); }
		  | Values { $$ = new ExprNode($1, ExprNode::value); }
		  | CallArrOrFunc { $$ = $1; }
		  ;
		  
CallArrOrFunc: IDENTIFIER '('OptEndl ExpressionList OptEndl')' { $$ = ExprNode::exprList(ExprNode::access_arr_or_call_func_params, $1, $4); }
			 | IDENTIFIER '('OptEndl')' { $$ = ExprNode::exprList(ExprNode::access_arr_or_call_func, $1, 0); }
			 ;
		
Values: STRING { $$ = new Value($1, Value::string_); }
	  | Boolean { $$ = $1; }
	  | DOUBLE { $$ = new Value($1, Value::Double_); }
	  | CHAR { $$ = new Value($1, Value::Char_); }
	  | DECIMAL_NUMBER { $$ = new Value($1, Value::dec_num, 1, 0); }
	  | IDENTIFIER { $$ = new Value($1, Value::id_); }
	  ;
	 
Boolean: KW_FALSE { $$ = new Value($1, Value::bool_); }
	   | KW_TRUE { $$ = new Value($1, Value::bool_); }
	   ;
		
FuncParamList: FuncParam { $$ = new FuncParamListNode($1); }
			 | FuncParamList ',' OptEndl FuncParam { $$ = FuncParamListNode::Append($1, $4); }
			 ;

FuncParam: LinkOrValEmpty IDENTIFIER ArrInParam As Type { }
		 ;

LinkOrVal: ByVal
		 | ByRef
		 | LinkOrVal ByRef
		 | LinkOrVal ByVal
		 ;

LinkOrValEmpty: 
			  | LinkOrVal
			  ;
		 
ArrInParam: '('OptEndl')'
		  | '('OptEndl Expression OptEndl')'
		  |
		  ;

FunctionDeclaration: Function IDENTIFIER '(' OptEndl ')' EndList OptStmtList END Function EndList { $$ = FuncDecl::funcDeclare($2, 0, 0, 0, $7); }
                   | Function IDENTIFIER '(' OptEndl ')' As Type EndList OptStmtList END Function EndList { $$ = FuncDecl::funcDeclare($2, $7, 0, 0, $9); }
                   | Function IDENTIFIER '(' OptEndl FuncParamList OptEndl')' EndList OptStmtList END Function EndList { $$ = FuncDecl::funcDeclare($2, 0, $5, 0, $9); }
                   | Function IDENTIFIER '(' OptEndl FuncParamList OptEndl')' As Type EndList OptStmtList END Function EndList { $$ = FuncDecl::funcDeclare($2, $9, $5, 0, $11); }
                   ;
				   
SubDeclaration: Sub IDENTIFIER '('OptEndl')' EndList OptStmtList END Sub EndList { $$ = FuncDecl::funcDeclare($2, 0, 0, 1, $7); }
              | Sub IDENTIFIER '('OptEndl FuncParamList OptEndl')' EndList OptStmtList END Sub EndList { $$ = FuncDecl::funcDeclare($2, 0, $5, 1, $9); }
              ;

IfStmt: IF Expression THEN EndList OptStmtList END IF { $$ = IfNode::IfClear($2, $5, IfNode::clear_); }
	| IF Expression THEN EndList OptStmtList ELSE EndList OptStmtList END IF { $$ = IfNode::IfElse($2, $5, $8, IfNode::else_); }
	| IF Expression THEN EndList OptStmtList ELSEIF Expression THEN EndList OptStmtList END IF { $$ = IfNode::IfElseIf($2, $5, $7, $10, IfNode::else_); }
	;

WhileStatement: WHILE Expression EndList OptStmtList END WHILE { $$ = While::whileStmt($2, $4, While::simple_); };
			
ContinueWhile: CONTINUE WHILE;

DoLoopUntilStatement: DO UNTIL Expression EndList OptStmtList LOOP { $$ = While::whileStmt($3, $5, While::doloopuntil); };
	
DOOption: EXITDO { $$ = StmtNode::DeclarationDoOption(StmtNode::dooption_exit); }
		| CONTINUEDO { $$ = StmtNode::DeclarationDoOption(StmtNode::dooption_continue); }
		;

DoLoopWhileStatement: DO WHILE Expression EndList OptStmtList LOOP { $$ = While::whileStmt($3, $5, While::doloopwhile_); };
	
EXITDO: EXIT DO;

CONTINUEDO: CONTINUE DO;

OptionalStep: { $$ = OptionalStep::addStep(0, false); }
			| STEP Expression { $$ = OptionalStep::addStepExpr($2, true); }
			;
			
ContinueExitFor: CONTINUE FOR { $$ = StmtNode::DeclarationContinueExitFor(StmtNode::continue_for); }
			   | EXIT FOR { $$ = StmtNode::DeclarationContinueExitFor(StmtNode::exit_for); }
			   ;

ForStatement: FOR Expression TO Expression OptionalStep EndList OptStmtList NEXT IDENTIFIER { $$ = ForNode::fornode($2, $4, $5, 0, $7); }
			;
				 
EndList: TOKEN_LINE
	   | EndList TOKEN_LINE
	   ;
	   
OptEndl: EndList
	   |
	   ;
	   
%%

void yyerror(char const *s)
{
    printf("%s\n",s);
}