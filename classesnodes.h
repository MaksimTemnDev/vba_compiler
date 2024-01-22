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
class SubDecl;
class FuncParamNode;
class FuncParamListNode;
class TypeNode;
class GlobalCodeList;
class GlobalCode;
class DimStmt;

class CodeNode{
    public: 
    int id;
    GlobalCodeList* globalCode = NULL;
    GlobalCodeList(GlobalCode *globalcode);
   void toDot(string &dot);
};

class GlobalCode{

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

    FuncDecl(string* name, TypeNode* returnType, FuncParamListNode* params, ExprNode* body);
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
        date_, int_, byte_, single, char_, string_, short_, bool_, obj_, decimal_
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

    b_plus, b_minus, b_div, b_mul, degree, int_div, mod_div, more, less, more_s, less_s, not_eq, bit_l_shift, bit_r_shift,
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