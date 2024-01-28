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