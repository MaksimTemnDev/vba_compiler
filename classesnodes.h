#pragma once
#include <iostream>
#include <string>
#include <list>
using namespace std;

class CodeNode;
class ExprNode;
class ExprListNode;
class StmtNode;
class StmtListNode;
class FuncDecl;
class FuncParamNode;
class FuncParamListNode;
class TypeNode;
class GlobalCodeList;
class GlobalCode;
class DimStmt;
class Value;
class ForNode;
class ArrayIdList;
class ArrayIdDeclare;
class Identificator;
class IdList;
class While;
class Ternar;
class IfNode;
class OptionalStep;
class BodyStmt;

class CodeNode{
    public: 
    int id;
    GlobalCodeList* globalCode = NULL;
    CodeNode(GlobalCodeList* globalcode);
   void toDot(std::string &dot);
};

class GlobalCode{
public:
    int id;
    FuncDecl* subfunc = NULL;
    DimStmt* dim = NULL;

    static GlobalCode* addDim(DimStmt* dim);
    static GlobalCode* addSubFunc(FuncDecl* subfunc);
    
    void toDot(std::string &dot);
};

class GlobalCodeList{
    public:
    int id;
    list<GlobalCode*>* globalCodes = NULL;

    GlobalCodeList(GlobalCode* globalCode);
    GlobalCodeList(GlobalCodeList* globalCodes);
    static GlobalCodeList* Append(GlobalCodeList* globalCodes, GlobalCode* globalCode);

    void toDot(std::string &dot);
};

class FuncDecl{
    public:
    int id;
    Identificator*  name = NULL;
    TypeNode* returnType = NULL;
    IdList* params = NULL;
    BodyStmt* body = NULL;
    StmtListNode* stmt_list = NULL;
    bool is_sub = false;

    static FuncDecl* funcDeclare(Identificator* name, TypeNode* returnType, IdList* params, BodyStmt* body, bool is_sub, StmtListNode* stmtList);

   void toDot(std::string &dot);
};

class FuncParamNode{
public:
    enum Type{
        date_, int_, byte_, single, char_, string_, short_, bool_, obj_, decimal_
    };
    int id;
    Type param_type;
    std::string* name = NULL;
    TypeNode* type = NULL;

    FuncParamNode(std::string* name, TypeNode* type, Type param_type);

   void toDot(std::string &dot);
};

class FuncParamListNode{
public:
    int id;
    list<FuncParamNode*>* items = NULL;

    FuncParamListNode(FuncParamNode* item);
    FuncParamListNode(FuncParamListNode* list);

   void toDot(std::string &dot);
};

class TypeNode{
public:
    enum Type{
        date_, int_, byte_, single, char_, string_, short_, bool_, obj_, decimal_, double_
    };

    int id;
    Type type;
    std::string *name;
    TypeNode* typeArr = NULL;
    ExprNode* exprArr = NULL;

    TypeNode(TypeNode* type);
    TypeNode(Type type, TypeNode* type_node, ExprNode* expr);
    TypeNode(Type type, std::string *name);
    void toDot(std::string &dot);

};

class ExprNode{
    public:
    enum Type{assign, no_assign_part, or_, or_elase, and_, and_also, plus_assign, minus_assign, mul_assign, 
    div_assign, expr_assign, bit_and_aassign, div_num_assign, bit_l_shift_assign, bit_r_shift_assign,

    b_plus, str_plus, b_minus, b_div, b_mul, degree, int_div, mod_div, more, less, more_s, less_s, _not_eq, bit_l_shift, bit_r_shift,
    u_plus, u_minus, not_, arr_body, arr_empty, arr_body_type, iif, array_access, like, is, isnot, typof, arr_expr_list, ternar,

    single, string_, bool_val, double_val, date_, char_val, obj, dec_num, int_val, byte_num, short_val, identifier, value, expr_start_func, values_with_id,
    expr_start_id

    };

    int id;
    Type type;
    char Char = 0;
    std::string* string = NULL;
    int Int = 0;
    double Double = 0;
    bool Bool;
    std::string *ParentID = NULL;
    std::string *Name = NULL;

    ExprNode* expr_left = NULL;
    ExprNode* expr_right = NULL;
    ExprListNode* expr_list = NULL;
    ExprNode* field_list = NULL;
    StmtListNode* stmt_list = NULL;
    IdList* id_list = NULL;
    TypeNode* type_node = NULL;
    Ternar* _ternar = NULL;
    Identificator* ident = NULL;
    Value* _value = NULL;

    list<ExprNode*>* ifList = NULL;
    ExprNode* else_body = NULL;
    ExprNode* body = NULL;
    ExprNode* condition = NULL;
    
    ExprNode* isnotis = NULL;

    ExprNode();
    ExprNode(Value* value, Type type);
    ExprNode(ExprNode* exprNode, Type type, TypeNode* typeNode);

    //Функции для работы
    static ExprNode* OperatorExpr(Type type, ExprNode* left, ExprNode* right);
    static ExprNode* OperatorIdExpr(Type type, Identificator* left, ExprNode* right);
    static ExprNode* IifExpr(Type type, ExprNode* condition, ExprNode* body, ExprNode* else_body);
    static ExprNode* typeOfisnotIs(Type type, ExprNode* isnotIs);
    static ExprNode* arrayBodyIdList(IdList* idList, Type type);
    static ExprNode* arrayBodyExprList(ExprListNode* exprList, Type type);
    static ExprNode* ternarOp(Ternar* ternar, Type type);
    static ExprNode* valueExpr(Type type, Identificator* ident, Value* value);
    static ExprNode* exprList(Type type, Identificator* ident, ExprListNode* expr_list);

    void toDot(std::string &dot, const std::string &pos = "");
};

class ExprListNode
{
public:
    int id;
    list<ExprNode*>* exprs = NULL;

    ExprListNode(ExprNode* expr);
    ExprListNode(ExprListNode* exprs);
    static ExprListNode* Append(ExprListNode* exprList, ExprNode* expr);

   void toDot(std::string &dot, const std::string &type="expr_list");
};

class StmtNode
{
public:
    enum Type
    {
        dim_, ifstmt_, while_, dowhile_, dountil_, for_, static_, expr_, continue_while, dooption_exit, dooption_continue, continue_for, exit_for
    };

    int id;
    Type item_type;
    ExprNode* expr;
    bool isStatic;
    DimStmt* dim;
    IfNode* ifNode;
    While* while_stmt;
    ForNode* forstmt;

    static StmtNode* DeclarationExpression(ExprNode* expr, Type item_type);
    static StmtNode* DeclarationDim(DimStmt* dim, Type item_type, bool isStatic);
    static StmtNode* DeclarationIf(IfNode* ifNode, Type item_type);
    static StmtNode* DeclarationWhile(While* while_stmt, Type item_type);
    static StmtNode* DeclarationFor(ForNode* forstmt, Type item_type);
    static StmtNode* DeclarationContinueWhile(Type item_type);
    static StmtNode* DeclarationDoOption(Type item_type);
    static StmtNode* DeclarationContinueExitFor(Type item_type);
    StmtNode();

   void toDot(std::string &dot);

};

class IfNode
{
public:
    enum Type{
        clear_, else_, ternar_, elseif
    };
    int id;
    ExprNode* condition;
    ExprNode* conditionElseIF;
    StmtListNode* stmtListNode;
    StmtListNode* stmtElse;
    Ternar* ternar;
    Type type;

    static IfNode* IfClear(ExprNode* exprNode, StmtListNode* stmtListNode, Type type);
    static IfNode* IfElse(ExprNode* exprNode, StmtListNode* stmtListNode, StmtListNode* stmtElseListNode, Type type);
    static IfNode* IfTernar(ExprNode* exprNode, Ternar* ternar, Type type);
    static IfNode* IfElseIf(ExprNode* exprNode, StmtListNode* stmtListNode, ExprNode* conditionElse, StmtListNode* stmtElseIfListNode, Type type);
    
    void toDot(std::string &dot);
};

class Ternar
{
public:
    int id;
    ExprNode* condition;
    ExprNode* yes;
    ExprNode* not_;

    static Ternar* ternarOp(ExprNode* cond, ExprNode* y, ExprNode* n);

    void toDot(std::string &dot);
};

class While
{
public:
    enum Type{
        simple_, doloopwhile_, doloopuntil
    };
    int id;
    ExprNode* condition;
    StmtListNode* body;
    Type type;

    static While* whileStmt(ExprNode* condition, StmtListNode* body, Type type);

    void toDot(std::string &dot);
};

class StaticDim
{
public:
    int id;
    DimStmt* dim;

    static StaticDim* DeclareStatic(DimStmt* dim);

    void toDot(std::string &dot);
};

class DimStmt
{
public:
    enum Type
    {
        single_expr, single_type, array_with, array_without
    };

    int id;
    Type type;
    ExprNode* exprNode = NULL;
    TypeNode* typeNode = NULL;
    IdList* idList = NULL;
    ArrayIdList* arrayIdList = NULL;
    bool isStatic = false;

    DimStmt();
    DimStmt(DimStmt* dimStmt);

    static DimStmt* DeclarationSingleType(IdList* idList, Type type, TypeNode* typeNode);
    static DimStmt* DeclarationSingleExpr(IdList* idList, Type type, ExprNode* exprNode);
    static DimStmt* DeclarationArray(ArrayIdList* arrayIdList, Type type, TypeNode* typeNode);
    void becomeStatic();

    void toDot(std::string &dot);
};

class IdList
{
public:
    int id;
    list<Identificator*>* identificators = NULL;

    IdList(Identificator* identificator);
    IdList(IdList* IdList);
    static IdList* Append(IdList* idList, Identificator* Identificator);
    void toDot(std::string &dot, const std::string &type="id_list");
};

class Identificator
{
public:
    enum Type
    {
        val_, var_, func_, arr_, class_
    };
    int id;
    std::string* identifier;
    Type type;
    Value* arrSize;
    ExprListNode* exprList;

    static Identificator* id_witout(std::string* identifier, Type type);
    static Identificator* id_with(std::string* identifier, Type type, Value* size);
    static Identificator* id_func(std::string* identifier, Type type, ExprListNode* exprs);
    static Identificator* id_witout(Identificator* identifier, Type type);
    static Identificator* id_with(Identificator* identifier, Type type, Value* size);
    static Identificator* id_func(Identificator* identifier, Type type, ExprListNode* exprs);

    void toDot(std::string &dot);
};

class ArrayIdDeclare
{
public:
    int id;
    std::string* identifier;
    int index;
    //И-тор переменнной внутри массива
    Identificator* input_id = NULL;

    ArrayIdDeclare(std::string* identifier, Identificator* input_id);

    void toDot(std::string &dot);
};

class ArrayIdList
{
public:
    int id;
    list<ArrayIdDeclare*>* arrayId = NULL;
    list<Identificator*>* arrayIdent = NULL;

    ArrayIdList(Identificator* ident);
    ArrayIdList(ArrayIdDeclare* arrayIdDeclare);
    ArrayIdList(ArrayIdList* arrayIdList);
    static ArrayIdList* Append(ArrayIdList* arrIdList, ArrayIdDeclare* arrayId);
    static ArrayIdList* Append(ArrayIdList* arrIdList, Identificator* ident);
    void toDot(std::string &dot, const std::string &type="arr_id_list");
};

class ForNode
{
public:
    int id;
    ExprNode* startExpr;
    ExprNode* endExpr;
    OptionalStep* step;
    ExprNode* assignExpVar;
    StmtListNode* body;

    static ForNode* fornode(ExprNode* startExpr, ExprNode* endExpr, OptionalStep* step, ExprNode* assignExpVar, StmtListNode* body);

    void toDot(std::string &dot);
};

class OptionalStep
{
public:
    int id;
    bool hasStep = false;
    Value* stepval = NULL;

    static OptionalStep* addStep(Value* stepval, bool hasStep);

    void toDot(std::string &dot);
};

class Value
{
public:
    enum Type
    {
    int_, byte_num, id_, short_, bool_, single_, Double_, string_, date_, Char_, obj_, dec_num
    };
    int id;
    int value;
    char char_;
    std::string str;
    double double_;
    bool Bool_;
    Identificator* identificator;
    Type type;
    bool hasIntVal = false;

    Value(Identificator* ident, Type type);
    Value(double double_, Type type);
    Value(std::string str, Type type);
    Value(char char_, Type type);
    Value(bool bool_, Type type);
    Value(int value, Type type, bool hasIntVal, Identificator* id);

    void toDot(std::string &dot);
};

class StmtListNode
{
public:
    int id;
    list<StmtNode*>* stmts = NULL;

    StmtListNode(StmtNode* stmtNode);
    static StmtListNode* Append(StmtListNode* stmtListNode, StmtNode* stmtNode);

    void toDot(std::string &dot, const std::string &type = "stmt_list");
};

class BodyStmt
{
public:
    int id;

    list<StmtNode*>* stmts = NULL;
    ExprNode* expr = NULL;

    BodyStmt(ExprNode* expr, StmtListNode* stmt);

    void toDot(std::string& dot);
};

void connectVerticesDots(std::string &s, int parentId, int childId);
void createVertexDot(std::string &s, int id, std::string name="", std::string type="", std::string value = "", std::string pos = "");