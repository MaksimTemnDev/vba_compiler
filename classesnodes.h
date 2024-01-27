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

class CodeNode{
    public: 
    int id;
    GlobalCodeList* globalCode = NULL;
    GlobalCodeList(GlobalCode* globalcode);
   void toDot(string &dot);
};

class GlobalCode{
public:
    int id;
    FuncDecl* subfunc = NULL;
    DimStmt* dim = NULL;

    static GlobalCode* addDim(DimStmt* dim);
    static GlobalCode* addSubFunc(FuncDecl* subfunc);
    
    void toDot(string &dot);
};

class GlobalCodeList{
    public:
    int id;
    list<GlobalCode*>* globalCodes = NULL;

    GlobalCodeList(GlobalCode* globalCode);
    GlobalCodeList(GlobalCodeList* globalCodes);
    static GlobalCodeList* Append(GlobalCodeList* globalCodes, GlobalCode* GlobalCode);

    void toDot(string &dot);
};

class FuncDecl{
    public:
    int id;
    string*  name = NULL;
    TypeNode* returnType = NULL;
    FuncParamListNode* params = NULL;
    ExprNode* body = NULL;
    bool is_sub = false;

    FuncDecl(string* name, TypeNode* returnType, FuncParamListNode* params, ExprNode* body, bool is_sub);

   void toDot(string &dot);
};

class FuncParamNode{
public:
    enum Type{
        date_, int_, byte_, single, char_, string_, short_, bool_, obj_, decimal_
    };
    int id;
    Type param_type;
    string* name = NULL;
    TypeNode* type = NULL;

    FuncParamNode(string* name, TypeNode* type, Type param_type);

   void toDot(string &dot);
};

class FuncParamListNode{
public:
    int id;
    list<FuncParamNode*>* items = NULL;

    FuncParamListNode(FuncParamNode* item);
    FuncParamListNode(FuncParamListNode* list);

   void toDot(string &dot);
};

class TypeNode{
public:
    enum Type{
        date_, int_, byte_, single, char_, string_, short_, bool_, obj_, decimal_, double_
    };

    int id;
    Type type;
    string *name;
    TypeNode* typeArr = NULL;
    ExprNode* exprArr = NULL;

    TypeNode(Type type);
    TypeNode(Type type, TypeNode* type_node, ExprNode* expr);
    TypeNode(Type type, string *name);
    void toDot(string &dot);

};

class ExprNode{
    public:
    enum Type{assign, no_assign_part, or, or_elase, and, and_also, plus_assign, minus_assign, mul_assign, 
    div_assign, expr_assign, bit_and_aassign, div_num_assign, bit_l_shift_assign, bit_r_shift_assign,

    b_plus, str_plus, b_minus, b_div, b_mul, degree, int_div, mod_div, more, less, more_s, less_s, not_eq, bit_l_shift, bit_r_shift,
    u_plus, u_minus, not, arr_body, arr_empty, arr_body_type, iif, array_access, like, is, isnot, typof,

    single, string, bool_val, double_val, date, char_val, obj, dec_num, int_val, byte_num, short_val, identifier 
    };

    int id;
    Type type;
    char Char = 0;
    string *String = NULL;
    int Int = 0;
    double Double = 0;
    bool Bool;
    string *ParentID = NULL;
    string *Name = NULL;

    ExprNode* expr_left = NULL;
    ExprNode* expr_right = NULL;
    ExprListNode* expr_list = NULL;
    ExprNode* field_list = NULL;
    StmtListNode* stmt_list = NULL;

    list<ExprNode*>* ifList = NULL;
    ExprNode* else_body = NULL;

    //Функции для работы
    static ExprNode* OperatorExpr(Type type, ExprNode* left, ExprNode* right);
    static ExprNode* IifExpr(Type type, ExprNode* condition, ExprNode* body, ExprNode* else_body);
    static ExprNode* typeOfisnotIs(Type type, ExprNode* isnotIs);

    void toDot(string &dot, const string &pos = "");
};

class ExprListNode
{
public:
    int id;
    list<ExprNode*>* exprs = NULL;

    ExprListNode(ExprNode* expr);
    ExprListNode(ExprListNode* exprs);

   void toDot(string &dot, const string &type="expr_list");
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

    static StmtNode* DeclarationExpression(ExprNode* expr, Type item_type);
    static StmtNode* DeclarationDim(DimStmt* dim, Type item_type, bool isStatic);
    static StmtNode* DeclarationIf(IfNode* ifNode, Type item_type);
    static StmtNode* DeclarationWhile(While* while_stmt, Type item_type);
    static StmtNode* DeclarationFor(ForNode* forstmt, Type item_type);
    static StmtNode* DeclarationContinueWhile(Type item_type);
    static StmtNode* DeclarationDoOption(Type item_type);
    static StmtNode* DeclarationContinueExitFor(Type item_type);
    StmtNode(StmtNode* node);
    StmtNode();

   void toDot(string &dot);

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

    static IfNode* IfClear(ExprNode* exprNode, StmtListNode* stmtListNode, Type type);
    static IfNode* IfElse(ExprNode* exprNode, StmtListNode* stmtListNode, StmtListNode* stmtElseListNode, Type type);
    static IfNode* IfTernar(ExprNode* exprNode, Ternar* ternar, Type type);
    static IfNode* IfElseIf(ExprNode* exprNode, StmtListNode* stmtListNode, ExprNode* conditionElse, StmtListNode* stmtElseIfListNode, Type type);
    
    void toDot(string &dot);
};

class Ternar
{
public:
    int id;
    ExprNode* condition;
    ExprNode* yes;
    ExprNode* not;

    static Ternar* ternarOp(ExprNode* cond, ExprNode* y, ExprNode* n);

    void toDot(string &dot);
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

    void toDot(string &dot);
};

class StaticDim
{
public:
    int id;
    DimStmt* dim;

    static StaticDim* DeclareStatic(DimStmt* dim);

    void toDot(string &dot);
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

    static DimStmt* DeclarationSingleType(IdList* idList, Type type, TypeNode* typeNode);
    static DimStmt* DeclarationSingleExpr(IdList* idList, Type type, ExprNode* exprNode);
    static DimStmt* DeclarationArray(ArrayIdList* arrayIdList, Type type, TypeNode* typeNode);
    void becomeStatic();

    void toDot(string &dot);
};

class IdList
{
public:
    int id;
    list<Identificator*>* identificators = NULL;

    IdList(Identificator* identificator);
    IdList(IdList* IdList);
    static IdList* Append(IdList* idList, Identificator* Identificator);
    void toDot(string &dot, const string &type="id_list");
};

class Identificator
{
public:
    enum Type
    {
        var_, func_, arr_, class_
    };
    int id;
    string* identifier;
    Type type;
    Value* arrSize;
    ExprListNode* exprList;

    static Identificator* id_witout(string* identifier, Type* type);
    static Identificator* id_with(string* identifier, Type* type, Value* size);
    static Identificator* id_func(string* identifier, Type* type, ExprListNode* exprs);

    void toDot(string &dot);
};

class ArrayIdDeclare
{
public:
    int id;
    string* identifier;
    int index;
    //И-тор переменнной внутри массива
    Identificator* input_id = NULL;

    ArrayIdDeclare(string* identifier, Identificator* input_id);

    void toDot(string &dot);
};

class ArrayIdList
{
public:
    int id;
    list<ArrayIdDeclare*>* arrayId = NULL;

    ArrayIdList(ArrayIdDeclare* arrayIdDeclare);
    ArrayIdList(ArrayIdList* arrayIdList);
    static ArrayIdList* Append(ArrayIdList* arrIdList, ArrayIdDeclare* arrayId);
    void toDot(string &dot, const string &type="arr_id_list");
};

class ForNode
{
public:
    int id;
    ExprNode* startExpr;
    ExprNode* endExpr;
    OptionalStep* step = false;
    ExprNode* assignExpVar;
    StmtListNode* body;

    static ForNode* fornode(ExprNode* startExpr, ExprNode* endExpr, OptionalStep* step, ExprNode* assignExpVar, StmtListNode* body);

    void toDot(string &dot);
};

class OptionalStep
{
public:
    int id;
    bool hasStep = false;
    Value* stepval = NULL;

    static OptionalStep* addStep(Value* stepval, bool hasStep);

    void toDot(string &dot);
};

class Value
{
public:
    enum Type
    {
    int_, byte_num, id_, short_, bool_, single_, double_, string_, date_, char_, obj_, dec_num
    };
    int id;
    int value;
    Identificator* identificator;
    Type type;
    bool hasIntVal = false;

    Value(int value, Type type, bool hasIntVal, Identificator* id);

    void toDot(string &dot);
};

class StmtListNode
{
public:
    int id;
    list<StmtNode*>* stmts = NULL;

    StmtListNode(StmtNode* stmtNode);
    static StmtListNode* Append(StmtListNode* stmtListNode, StmtNode* stmtNode);

    void toDot(string &dot, const string &type = "stmt_list");
};

void connectVerticesDots(string &s, int parentId, int childId);
void createVertexDot(string &s, int id, string name="", string type="", string value = "", string pos = "");