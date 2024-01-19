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
    GlobalCodeList(GlobalCode* globalcode);
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