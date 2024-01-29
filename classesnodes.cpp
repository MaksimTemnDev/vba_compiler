#include "classesnodes.h"
int globId = 0;

CodeNode::CodeNode(GlobalCodeList* globalcode){
    this->id = ++globId;
    this->globalCode = globalcode;
}

GlobalCode* GlobalCode::addDim(DimStmt* dim){
    GlobalCode* new_code = new GlobalCode();
    new_code->id = ++globId;
    new_code->dim = dim;
    return new_code;
}

GlobalCode* GlobalCode::addSubFunc(FuncDecl* subfunc){
    GlobalCode* new_code = new GlobalCode();
    new_code->id = ++globId;
    new_code->subfunc = subfunc;
    return new_code;
}

GlobalCodeList::GlobalCodeList(GlobalCode* globalCode){
    this->id = ++globId;
    this->globalCodes = new list<GlobalCode*>{ globalCode };
}

GlobalCodeList::GlobalCodeList(GlobalCodeList* globalCodes){
    this->id = ++globId;
    if(globalCodes != NULL)
    {
        this->globalCodes = globalCodes->globalCodes;
    }
    else
    {
        this->globalCodes = new std::list<GlobalCode*>;
    }
}

GlobalCodeList* GlobalCodeList::Append(GlobalCodeList* globalCodes, GlobalCode* globalCode){
    globalCodes->globalCodes->push_back(globalCode);
    return globalCodes;
}

FuncDecl* FuncDecl::funcDeclare(Identificator* name, TypeNode* returnType, IdList* params, StmtListNode* body, bool is_sub){
    FuncDecl* func = new FuncDecl();
    func->name = name;
    func->params = params;
    func->body = body;
    func->is_sub = is_sub;
    func->id = ++globId;
    return func;
}

void FuncDecl::addBody(StmtListNode* body){
    this->body = body;
}

void FuncDecl::addReturn(ExprNode* return_){
    this->return_ = return_;
}

TypeNode::TypeNode(Type type){
    this->id = ++globId;
    this->type = type;
}

TypeNode::TypeNode(Type type, TypeNode* type_node, ExprNode* expr){
    this->id = ++globId;
    this->type = type;
    this->typeArr = typeArr;
    this->exprArr = expr;
}

TypeNode::TypeNode(Type type, string *name){
    this->id = ++globId;
    this->type = type;
    this->name = name;
}

ExprNode* ExprNode::OperatorExpr(Type type, ExprNode* left, ExprNode* right){
    ExprNode* new_expr = new ExprNode();
    new_expr->type = type;
    new_expr->expr_left = left;
    new_expr->expr_right = right;
    return new_expr;
}

ExprNode* ExprNode::IifExpr(Type type, ExprNode* condition, ExprNode* body, ExprNode* else_body){
    ExprNode* new_expr = new ExprNode();
    new_expr->type = type;
    new_expr->body = body;
    new_expr->condition = condition;
    new_expr->else_body = else_body;
    return new_expr;
}

ExprNode* ExprNode::typeOfisnotIs(Type type, ExprNode* isnotIs){
    ExprNode* new_expr = new ExprNode();
    new_expr->type = type;
    new_expr->isnotis = isnotIs;
    return new_expr;
}

ExprListNode::ExprListNode(ExprNode* expr){
    this->id = ++globId;
    this->exprs = new list<ExprNode*>{expr};
}

ExprListNode::ExprListNode(ExprListNode* exprs){
    this->id = ++globId;
    if(exprs != NULL)
    {
        this->exprs = exprs->exprs;
    }
    else
    {
        this->exprs = new std::list<ExprNode*>;
    }
}

StmtNode* StmtNode::DeclarationExpression(ExprNode* expr, Type item_type){
    StmtNode* new_stmt = new StmtNode();
    new_stmt->id = ++globId;
    new_stmt->expr = expr;
    new_stmt->item_type = item_type;
    return new_stmt;
}

StmtNode* StmtNode::DeclarationDim(DimStmt* dim, Type item_type, bool isStatic){
    StmtNode* new_stmt = new StmtNode();
    new_stmt->id = ++globId;
    new_stmt->dim = dim;
    new_stmt->isStatic = isStatic;
    new_stmt->item_type = item_type;
    return new_stmt;
}

StmtNode* StmtNode::DeclarationIf(IfNode* ifNode, Type item_type){
    StmtNode* new_stmt = new StmtNode();
    new_stmt->id = ++globId;
    new_stmt->ifNode = ifNode;
    new_stmt->item_type = item_type;
    return new_stmt;
}

StmtNode* StmtNode::DeclarationWhile(While* while_stmt, Type item_type){
    StmtNode* new_stmt = new StmtNode();
    new_stmt->id = ++globId;
    new_stmt->while_stmt = while_stmt;
    new_stmt->item_type = item_type;
    return new_stmt;
}

StmtNode* StmtNode::DeclarationFor(ForNode* forstmt, Type item_type){
    StmtNode* new_stmt = new StmtNode();
    new_stmt->id = ++globId;
    new_stmt->forstmt = forstmt;
    new_stmt->item_type = item_type;
    return new_stmt;
}

StmtNode* StmtNode::DeclarationContinueWhile(Type item_type){
    StmtNode* new_stmt = new StmtNode();
    new_stmt->id = ++globId;
    new_stmt->item_type = item_type;
    return new_stmt;
}

StmtNode* StmtNode::DeclarationDoOption(Type item_type){
    StmtNode* new_stmt = StmtNode::DeclarationContinueWhile(item_type);
    return new_stmt;
}

StmtNode* StmtNode::DeclarationContinueExitFor(Type item_type){
    StmtNode* new_stmt = StmtNode::DeclarationContinueWhile(item_type);
    return new_stmt;
}

StmtNode::StmtNode(){
    this->id = ++globId;
}

IfNode* IfNode::IfClear(ExprNode* exprNode, StmtListNode* stmtListNode, Type type){
    IfNode* new_if = new IfNode();
    new_if->id = ++globId;
    new_if->condition = exprNode;
    new_if->stmtListNode = stmtListNode;
    new_if->type = type;
    return new_if;
}

IfNode* IfNode::IfElse(ExprNode* exprNode, StmtListNode* stmtListNode, StmtListNode* stmtElseListNode, Type type){
    IfNode* new_if = new IfNode();
    new_if->id = ++globId;
    new_if->condition = exprNode;
    new_if->stmtListNode = stmtListNode;
    new_if->type = type;
    new_if->stmtElse = stmtElseListNode;
    return new_if;
}

IfNode* IfNode::IfTernar(ExprNode* exprNode, Ternar* ternar, Type type){
    IfNode* new_if = new IfNode();
    new_if->id = ++globId;
    new_if->condition = exprNode;
    new_if->ternar = ternar;
    new_if->type = type;
    return new_if;
}

IfNode* IfNode::IfElseIf(ExprNode* exprNode, StmtListNode* stmtListNode, ExprNode* conditionElse, StmtListNode* stmtElseIfListNode, Type type){
    IfNode* new_if = new IfNode();
    new_if->id = ++globId;
    new_if->condition = exprNode;
    new_if->stmtListNode = stmtListNode;
    new_if->type = type;
    new_if->conditionElseIF = conditionElse;
    new_if->stmtElse = stmtElseIfListNode;
    return new_if;
}

Ternar* Ternar::ternarOp(ExprNode* cond, ExprNode* y, ExprNode* n){
    Ternar* ternar = new Ternar();
    ternar->id = ++globId;
    ternar->condition = cond;
    ternar->yes = y;
    ternar->not_ = n;
    return ternar;
}

While* While::whileStmt(ExprNode* condition, StmtListNode* body, Type type){
    While* while_stmt = new While();
    while_stmt->id = ++globId;
    while_stmt->body = body;
    while_stmt->type = type;
    while_stmt->condition = condition;
    return while_stmt;
}

StaticDim* StaticDim::DeclareStatic(DimStmt* dim){
    StaticDim* _static = new StaticDim();
    _static->id = ++globId;
    _static->dim = dim;
    return _static;
}

DimStmt* DimStmt::DeclarationSingleType(IdList* idList, Type type, TypeNode* typeNode){
    DimStmt* dim = new DimStmt();
    dim->id = ++globId;
    dim->idList = idList;
    dim->type = type;
    dim->typeNode = typeNode;
    return dim;
}

DimStmt* DimStmt::DeclarationSingleExpr(IdList* idList, Type type, ExprNode* exprNode){
    DimStmt* dim = new DimStmt();
    dim->id = ++globId;
    dim->idList = idList;
    dim->type = type;
    dim->exprNode = exprNode;
    return dim;
}

DimStmt* DimStmt::DeclarationArray(ArrayIdList* arrayIdList, Type type, TypeNode* typeNode){
    DimStmt* dim = new DimStmt();
    dim->id = ++globId;
    dim->arrayIdList = arrayIdList;
    dim->type = type;
    dim->typeNode = typeNode;
    return dim;
}

void DimStmt::becomeStatic(){
    this->isStatic?false:true;
}

IdList::IdList(Identificator* identificator) {
    this->id = ++globId;
    this->identificators = new list<Identificator*>{ identificator };
}

IdList::IdList(IdList* IdList) {
    this->id = ++globId;
    if (IdList != NULL)
    {
        this->identificators = IdList->identificators;
    }
    else
    {
        this->identificators = new std::list<Identificator*>;
    }
}

IdList* IdList::Append(IdList* idList, Identificator* Identificator) {
    idList->identificators->push_back(Identificator);
    return idList;
}

Identificator* Identificator::id_witout(string* identifier, Type* type) {
    Identificator* id = new Identificator();
    id->id = ++globId;
    id->identifier = identifier;
    id->type = type;
    return id;
}

Identificator* Identificator::id_with(string* identifier, Type* type, Value* size) {
    Identificator* id = new Identificator();
    id->id = ++globId;
    id->identifier = identifier;
    id->type = type;
    id->arrSize = size;
    return id;
}

Identificator* Identificator::id_func(string* identifier, Type* type, ExprListNode* exprs) {
    Identificator* id = new Identificator();
    id->id = ++globId;
    id->identifier = identifier;
    id->type = type;
    id->exprList = exprs;
    return id;
}

ArrayIdDeclare::ArrayIdDeclare(string* identifier, Identificator* input_id) {
    this->id = ++globId;
    this->identifier = identifier;
    this->input_id = input_id;
}

ArrayIdList::ArrayIdList(ArrayIdDeclare* arrayIdDeclare) {
    this->id = ++globId;
    this->arrayId = new list<ArrayIdDeclare*>{ arrayIdDeclare };
}

ArrayIdList::ArrayIdList(ArrayIdList* arrayIdList) {
    this->id = ++globId;
    if (arrayIdList != NULL)
    {
        this->arrayId = ArrayIdList->arrayId;
    }
    else
    {
        this->arrayId = new std::list<ArrayIdDeclare*>;
    }
}

ArrayIdList* ArrayIdList::Append(ArrayIdList* arrIdList, ArrayIdDeclare* arrayId) {
    arrIdList->arrayId->push_back(arrayId);
    return arrIdList;
}

ForNode* ForNode::fornode(ExprNode* startExpr, ExprNode* endExpr, OptionalStep* step, ExprNode* assignExpVar, StmtListNode* body) {
    ForNode forNode = new ForNode();
    forNode->id = ++globId;
    forNode->startExpr = startExpr;
    forNode->endExpr = endExpr;
    forNode->step = step;
    forNode->assignExpVar = assignExpVar;
    forNode->body = body;
    return forNode;
}

OptionalStep* OptionalStep::addStep(Value* stepval, bool hasStep) {
    OptionalStep optStep = new OptionalStep();
    optStep->id = ++globId;
    optStep->stepval = stepval;
    optStep->hasStep = hasStep;
    return optStep;
}

Value::Value(int value, Type type, bool hasIntVal, Identificator* id) {
    this->id = ++globId;
    this->value = value;
    this->type = type;
    this->hasIntVal = hasIntVal;
    this->identificator = id;
}

StmtListNode::StmtListNode(StmtNode* stmtNode) {
    this->id = ++globId;
    this->stmts = new list<StmtNode*>{ stmtNode };
}

StmtListNode* StmtListNode::Append(StmtListNode* stmtListNode, StmtNode* stmtNode) {
    stmtListNode->stmts->push_back(stmtNode);
    return stmtListNode;
}

//toDot �������

void CodeNode::toDot(string& dot) {
    dot = "digraph vbCode {\n";
    createVertexDot(dot, this->id, "code");
    
    if (this->globalCode != NULL) {
        connectVerticesDots(dot, this->id, this->globalCode->id);
        this->globalCode->toDot(dot);
    }

    dot += "}\n";
}

void GlobalCode::toDot(string& dot) {
    createVertexDot(dot, this->id, "global_code", type, value, "", "");

    if (this->subfunc != NULL) {
        connectVerticesDots(dot, this->id, this->subfunc->id);
        this->subfunc->toDot(dot, "sub_func");
    }

    if (this->dim != NULL) {
        connectVerticesDots(dot, this->id, this->dim->id);
        this->dim->toDot(dot, "dim");
    }
}

void GlobalCodeList::toDot(string& dot) {
    createVertexDot(dot, this->id, "global_code_list");

    for (auto elem : *this->globalCodes)
    {
        int exprNum = 1;
        connectVerticesDots(dot, this->id, elem->id);
        elem->toDot(dot, "condition" + to_string(exprNum++));
    }
}

void FuncDecl::toDot(string& dot) {
    createVertexDot(dot, this->id, "func_decl", type, "", "");

    if (this->name != NULL) {
        connectVerticesDots(dot, this->id, this->name->id);
        this->name->toDot(dot, "name");
    }

    if (this->returnType != NULL) {
        connectVerticesDots(dot, this->id, this->returnType->id);
        this->returnType->toDot(dot, "return_type");
    }

    if (this->params != NULL) {
        connectVerticesDots(dot, this->id, this->params->id);
        this->params->toDot(dot, "params");
    }

    if (this->body != NULL) {
        connectVerticesDots(dot, this->id, this->body->id);
        this->body->toDot(dot, "body");
    }

    if (this->return_ != NULL) {
        connectVerticesDots(dot, this->id, this->return_->id);
        this->return_->toDot(dot, "return");
    }
}

void TypeNode::toDot(string& dot) {
    switch (this->type) {
        case TypeNode::date_:
            createVertexDot(dot, this->id, "date_type", "", "");
            break;

        case TypeNode::int_:
            createVertexDot(dot, this->id, "int_type", "", "");
            break;

        case TypeNode::byte_:
            createVertexDot(dot, this->id, "byte_type", "", "");
            break;

        case TypeNode::single:
            createVertexDot(dot, this->id, "single_type", "", "");
            break;

        case TypeNode::char_:
            createVertexDot(dot, this->id, "char_type", "", "");
            break;

        case TypeNode::string_:
            createVertexDot(dot, this->id, "string_type", "", "");
            break;

        case TypeNode::short_:
            createVertexDot(dot, this->id, "short_type", "", "");
            break;

        case TypeNode::bool_:
            createVertexDot(dot, this->id, "bool_type", "", "");
            break;

        case TypeNode::obj_:
            createVertexDot(dot, this->id, "obj_type", "", "");
            break;

        case TypeNode::decimal_:
            createVertexDot(dot, this->id, "decimal_type", "", "");
            break;

        case TypeNode::double_:
            createVertexDot(dot, this->id, "double_type", "", "");
            break;
    }

    createVertexDot(dot, this->id, "type", type, value, "", "");

    if (this->typeArr != NULL) {
        connectVerticesDots(dot, this->id, this->typeArr->id);
        this->typeArr->toDot(dot, "expr_left");
    }

    if (this->exprArr != NULL) {
        connectVerticesDots(dot, this->id, this->exprArr->id);
        this->exprArr->toDot(dot, "expr_right");
    }
}

void ExprNode::toDot(string& dot, const string& pos = "") {
    string type = "";
    string value = "";

    switch (this->type){
        case ExprNode::assign:
            type = "assign";
            break;
        
        case ExprNode::no_assign_part:
            type = "no_assign_part";
            break;

        case ExprNode::or_:
            type = "or";
            break;

        case ExprNode::or_elase:
            type = "or_else";
            break;

        case ExprNode::and_:
            type = "and";
            break;

        case ExprNode::and_also:
            type = "and_also";
            break;

        case ExprNode::plus_assign:
            type = "plus_assign";
            break;

        case ExprNode::minus_assign:
            type = "minus_assign";
            break;

        case ExprNode::mul_assign:
            type = "mul_assign";
            break;

        case ExprNode::div_assign:
            type = "div_assign";
            break;

        case ExprNode::expr_assign:
            type = "expr_assign";
            break;

        case ExprNode::bit_and_aassign:
            type = "bit_and_assign";
            break;

        case ExprNode::div_num_assign:
            type = "div_num_assign";
            break;

        case ExprNode::bit_l_shift_assign:
            type = "bit_l_shift_assign";
            break;

        case ExprNode::bit_r_shift_assign:
            type = "bit_r_shift_assign";
            break;

        case ExprNode::b_plus:
            type = "b_plus";
            break;

        case ExprNode::str_plus:
            type = "str_plus";
            break;

        case ExprNode::b_minus:
            type = "b_minus";
            break;

        case ExprNode::b_div:
            type = "b_div";
            break;

        case ExprNode::b_mul:
            type = "b_mul";
            break;

        case ExprNode::degree:
            type = "degree";
            break;

        case ExprNode::int_div:
            type = "int_div";
            break;

        case ExprNode::mod_div:
            type = "mod_div";
            break;

        case ExprNode::more:
            type = "more";
            break;

        case ExprNode::less:
            type = "less";
            break;

        case ExprNode::more_s:
            type = "more_s";
            break;

        case ExprNode::less_s:
            type = "less_s";
            break;

        case ExprNode::_not_eq:
            type = "not_eq";
            break;

        case ExprNode::bit_l_shift:
            type = "bit_l_shift";
            break;

        case ExprNode::bit_r_shift:
            type = "bit_r_shift";
            break;

        case ExprNode::u_plus:
            type = "u_plus";
            break;

        case ExprNode::u_minus:
            type = "u_minus";
            break;

        case ExprNode::not_:
            type = "not";
            break;

        case ExprNode::arr_body:
            type = "arr_body";
            break;

        case ExprNode::arr_empty:
            type = "arr_empty";
            break;

        case ExprNode::arr_body_type:
            type = "arr_body_type";
            break;

        case ExprNode::iif:
            type = "iif";
            break;

        case ExprNode::array_access:
            type = "array_access";
            break;

        case ExprNode::like:
            type = "like";
            break;

        case ExprNode::is:
            type = "is";
            break;

        case ExprNode::isnot:
            type = "isnot";
            break;

        case ExprNode::typof:
            type = "typeOf";
            break;

        case ExprNode::single:
            type = "single";
            break;

        case ExprNode::string_:
            type = "string";
            break;

        case ExprNode::bool_val:
            type = "bool_val";
            break;

        case ExprNode::double_val:
            type = "double_val";
            break;

        case ExprNode::date_:
            type = "date";
            break;

        case ExprNode::char_val:
            type = "char_val";
            break;

        case ExprNode::obj:
            type = "obj";
            break;

        case ExprNode::dec_num:
            type = "decimal_num";
            break;

        case ExprNode::int_val:
            type = "int_val";
            break;

        case ExprNode::byte_num:
            type = "byte_num";
            break;

        case ExprNode::short_val:
            type = "short_val";
            break;

        case ExprNode::identifier:
            type = "identifier";
            break;
    }

    createVertexDot(dot, this->id, "expr", type, value, "", pos);

    if (this->expr_left != NULL) {
        connectVerticesDots(dot, this->id, this->expr_left->id);
        this->expr_left->toDot(dot, "expr_left");
    }

    if (this->expr_right != NULL) {
        connectVerticesDots(dot, this->id, this->expr_right->id);
        this->expr_right->toDot(dot, "expr_right");
    }

    if (this->expr_list != NULL) {
        connectVerticesDots(dot, this->id, this->expr_list->id);
        this->expr_list->toDot(dot);
    }

    if (this->field_list != NULL) {
        connectVerticesDots(dot, this->id, this->field_list->id);
        this->field_list->toDot(dot, "field_list");;
    }

    if (this->stmt_list != NULL) {
        connectVerticesDots(dot, this->id, this->stmt_list->id);
        this->stmt_list->toDot(dot);
    }

    if (this->ifList != NULL) {

        int ifCount = 1;

        for (auto elem : *this->ifList)
        {
            connectVerticesDots(dot, this->id, elem->id);
            elem->toDot(dot, "condition" + to_string(ifCount++));
        }
    }

    if (this->else_body != NULL) {
        connectVerticesDots(dot, this->id, this->else_body->id);
        this->else_body->toDot(dot, "else_body");
    }

    if (this->body != NULL) {
        connectVerticesDots(dot, this->id, this->body->id);
        this->body->toDot(dot, "body");
    }

    if (this->condition != NULL) {
        connectVerticesDots(dot, this->id, this->condition->id);
        this->body->toDot(dot, "condition");
    }

    if (this->isnotis != NULL) {
        connectVerticesDots(dot, this->id, this->isnotis->id);
        this->body->toDot(dot, "isnotis");
    }
}

void ExprListNode::toDot(string& dot, const string& type = "expr_list") {
    createVertexDot(dot, this->id, "expr_list", type);

    for (auto elem : *this->exprs)
    {
        int exprNum = 1;
        connectVerticesDots(dot, this->id, elem->id);
        elem->toDot(dot, "condition" + to_string(exprNum++));
    }
}

void StmtNode::toDot(string& dot) {
    string type = "";
    string value = "";

    switch (this->type) {
        case dim_:
            type = "dim";
            break;

        case ifstmt_:
            type = "ifstmt";
            break;

        case while_:
            type = "while";
            break;

        case dowhile_:
            type = "dowhile";
            break;

        case dountil_:
            type = "dountil";
            break;
            
        case for_:
            type = "for";
            break;

        case static_:
            type = "static";
            break;

        case expr_:
            type = "expr";
            break;

        case continue_while:
            type = "continue_while";
            break;

        case dooption_exit:
            type = "dooption_exit";
            break;

        case dooption_continue:
            type = "dooption_continue";
            break;

        case continue_for:
            type = "continue_for";
            break;

        case exit_for:
            type = "exit_for";
            break;
    }

    createVertexDot(dot, this->id, "stmt", type, value);

    if (this->expr != NULL) {
        connectVerticesDots(dot, this->id, this->expr->id);
        this->expr->toDot(dot, "expr");
    }

    if (this->dim != NULL) {
        connectVerticesDots(dot, this->id, this->dim->id);
        this->dim->toDot(dot, "dim");
    }

    if (this->ifNode != NULL) {
        connectVerticesDots(dot, this->id, this->ifNode->id);
        this->ifNode->toDot(dot, "if");
    }

    if (this->while_stmt != NULL) {
        connectVerticesDots(dot, this->id, this->while_stmt->id);
        this->while_stmt->toDot(dot, "while");
    }

    if (this->forstmt != NULL) {
        connectVerticesDots(dot, this->id, this->forstmt->id);
        this->forstmt->toDot(dot, "for");
    }
}

void StmtListNode::toDot(string& dot, const string& type = "stmt_list") {
    createVertexDot(dot, this->id, "stmt_list", type);

    for (auto elem : *this->stmts)
    {
        int exprNum = 1;
        connectVerticesDots(dot, this->id, elem->id);
        elem->toDot(dot);
    }
}

void IfNode::toDot(string& dot) {
    string type = "";

    switch (this->type) {
        case clear_:
            type = "clear";
            break;

        case else_:
            type = "else";
            break;

        case ternar_:
            type = "ternar";
            break;

        case elseif:
            type = "elseif";
            break;
    }

    createVertexDot(dot, this->id, "if_stmt", "", "");

    if (this->condition != NULL) {
        connectVerticesDots(dot, this->id, this->condition->id);
        this->condition->toDot(dot);
    }

    if (this->conditionElseIF != NULL) {
        connectVerticesDots(dot, this->id, this->conditionElseIF->id);
        this->conditionElseIF->toDot(dot);
    }

    if (this->stmtListNode != NULL) {
        connectVerticesDots(dot, this->id, this->stmtListNode->id);
        this->stmtListNode->toDot(dot);
    }

    if (this->stmtElse != NULL) {
        connectVerticesDots(dot, this->id, this->stmtElse->id);
        this->stmtElse->toDot(dot);
    }

    if (this->ternar != NULL) {
        connectVerticesDots(dot, this->id, this->ternar->id);
        this->ternar->toDot(dot);
    }
}

void Ternar::toDot(string& dot) {
    createVertexDot(dot, this->id, "ternar_stmt", "", "");

    if (this->condition != NULL) {
        connectVerticesDots(dot, this->id, this->condition->id);
        this->condition->toDot(dot);
    }

    if (this->yes != NULL) {
        connectVerticesDots(dot, this->id, this->yes->id);
        this->yes->toDot(dot);
    }

    if (this->not_ != NULL) {
        connectVerticesDots(dot, this->id, this->not_->id);
        this->not_->toDot(dot);
    }
}

void While::toDot(string& dot) {
    string type = "";

    switch (this->type) {
    case simple_:
        type = "simple";
        break;

    case doloopwhile_:
        type = "doloopwhile";
        break;

    case doloopuntil:
        type = "doloopuntil";
        break;
    }

    createVertexDot(dot, this->id, "while_stmt", "", "");

    if (this->condition != NULL) {
        connectVerticesDots(dot, this->id, this->condition->id);
        this->condition->toDot(dot);
    }

    if (this->body != NULL) {
        connectVerticesDots(dot, this->id, this->body->id);
        this->body->toDot(dot);
    }
}

void StaticDim::toDot(string& dot) {
    createVertexDot(dot, this->id, "static_dim", "", "");

    if (this->dim != NULL) {
        connectVerticesDots(dot, this->id, this->dim->id);
        this->dim->toDot(dot);
    }
}

void DimStmt::toDot(string& dot) {
    string type = "";

    switch (this->type) {
        case single_expr:
            type = "single_expr";
            break;

        case single_type:
            type = "single_type";
            break;

        case array_with:
            type = "array_with";
            break;

        case array_without:
            type = "array_without";
            break;
    }

    createVertexDot(dot, this->id, "dim_stmt", "", "");

    if (this->exprNode != NULL) {
        connectVerticesDots(dot, this->id, this->exprNode->id);
        this->exprNode->toDot(dot);
    }

    if (this->typeNode != NULL) {
        connectVerticesDots(dot, this->id, this->typeNode->id);
        this->typeNode->toDot(dot);
    }

    if (this->idList != NULL) {
        connectVerticesDots(dot, this->id, this->idList->id);
        this->idList->toDot(dot);
    }

    if (this->arrayIdList != NULL) {
        connectVerticesDots(dot, this->id, this->arrayIdList->id);
        this->arrayIdList->toDot(dot);
    }
}

void IdList::toDot(string& dot, const string& type = "id_list") {
    createVertexDot(dot, this->id, "id_list");

    for (auto elem : *this->items)
    {
        int exprNum = 1;
        connectVerticesDots(dot, this->id, elem->id);
        elem->toDot(dot);
    }
}

void Identificator::toDot(string& dot) {
    string type = "";

    switch (this->type) {
        case val_:
            type = "val";
            break;

        case var_:
            type = "var";
            break;

        case func_:
            type = "func";
            break;

        case arr_:
            type = "arr";
            break;

        case class_:
            type = "class";
            break;
    }

    createVertexDot(dot, this->id, "identificator", "", "");

    if (this->arrSize != NULL) {
        connectVerticesDots(dot, this->id, this->arrSize->id);
        this->arrSize->toDot(dot);
    }

    if (this->exprList != NULL) {
        connectVerticesDots(dot, this->id, this->exprList->id);
        this->exprList->toDot(dot);
    }
}

void ArrayIdDeclare::toDot(string& dot) {
    createVertexDot(dot, this->id, "array_id_decl", "", "");

    if (this->input_id != NULL) {
        connectVerticesDots(dot, this->id, this->input_id->id);
        this->input_id->toDot(dot);
    }
}

void ArrayIdList::toDot(string& dot, const string& type = "arr_id_list") {
    createVertexDot(dot, this->id, "arr_id_list");

    for (auto elem : *this->items)
    {
        int exprNum = 1;
        connectVerticesDots(dot, this->id, elem->id);
        elem->toDot(dot);
    }
}

void ForNode::toDot(string& dot) {
    createVertexDot(dot, this->id, "for", "", "");

    if (this->startExpr != NULL) {
        connectVerticesDots(dot, this->id, this->startExpr->id);
        this->startExpr->toDot(dot);
    }

    if (this->endExpr != NULL) {
        connectVerticesDots(dot, this->id, this->endExpr->id);
        this->endExpr->toDot(dot);
    }

    if (this->step != NULL) {
        connectVerticesDots(dot, this->id, this->step->id);
        this->step->toDot(dot);
    }

    if (this->assignExpVar != NULL) {
        connectVerticesDots(dot, this->id, this->assignExpVar->id);
        this->assignExpVar->toDot(dot);
    }

    if (this->body != NULL) {
        connectVerticesDots(dot, this->id, this->body->id);
        this->body->toDot(dot);
    }
}

void OptionalStep::toDot(string& dot) {
    createVertexDot(dot, this->id, "opt_step", "", "");

    if (this->stepval != NULL) {
        connectVerticesDots(dot, this->id, this->stepval->id);
        this->stepval->toDot(dot);
    }
}

void Value::toDot(string& dot) {
    string type = "";

    switch (this->type) {
        case int_:
            type = "int";
            break;

        case byte_num:
            type = "byte_num";
            break;

        case id_:
            type = "id";
            break;

        case short_:
            type = "short";
            break;

        case bool_:
            type = "bool";
            break;

        case single_:
            type = "single";
            break;

        case double_:
            type = "double";
            break;

        case string_:
            type = "string";
            break;

        case date_:
            type = "date";
            break;

        case char_:
            type = "char";
            break;

        case obj_:
            type = "obj";
            break;

        case dec_num:
            type = "dec_num";
            break;
    }

    createVertexDot(dot, this->id, "value", "", "");

    if (this->identificator != NULL) {
        connectVerticesDots(dot, this->id, this->identificator->id);
        this->identificator->toDot(dot);
    }
}

void connectVerticesDots(string& s, int parentId, int childId) {
    string tmp = "id" + to_string(parentId) + " -> " + "id" + to_string(childId) + ";\n";
    s += tmp;
}

void createVertexDot(string& s, int id, string name, string type, string value, string visibility, string pos) {
    if (!type.empty()) {
        type = "type=" + type + " ";
    }

    if (!value.empty()) {
        value = "value=" + value + " ";
    }

    if (!visibility.empty()) {
        visibility = "visibility=" + visibility + " ";
    }

    if (!pos.empty())
    {
        pos = "position=" + pos + " ";
    }

    string tmp = "id" + to_string(id) +
        " [label=\"" + name + " " + type + value + visibility + pos + "id=" + to_string(id) + "\"];\n";

    s += tmp;
}