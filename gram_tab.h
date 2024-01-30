#pragma once
#include "classesnodes.h"
typedef union {
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
} YYSTYPE;
#define	STRING	258
#define	IDENTIFIER	259
#define	DOUBLE	260
#define	CHAR	261
#define	DECIMAL_NUMBER	262
#define	END	263
#define	WHILE	264
#define	DO	265
#define	LOOP	266
#define	UNTIL	267
#define	FOR	268
#define	TO	269
#define	STEP	270
#define	CONTINUE	271
#define	EXIT	272
#define	IF	273
#define	THEN	274
#define	ELSE	275
#define	ELSEIF	276
#define	DIM	277
#define	NEW	278
#define	AS	279
#define	RETURN	280
#define	NEXT	281
#define	TYPE_BOOLEAN	282
#define	TYPE_BYTE	283
#define	TYPE_INTEGER	284
#define	TYPE_SINGLE	285
#define	TYPE_SHORT	286
#define	TYPE_DOUBLE	287
#define	TYPE_DECIMAL	288
#define	TYPE_DATE	289
#define	TYPE_CHAR	290
#define	TYPE_STRING	291
#define	TYPE_OBJECT	292
#define	TOKEN_LINE	293
#define	Function	294
#define	Sub	295
#define	Iif	296
#define	KW_STATIC	297
#define	KW_FALSE	298
#define	KW_TRUE	299
#define	PLUS_ASSIGNMENT	300
#define	MINUS_ASSIGNMENT	301
#define	MUL_ASSIGNMENT	302
#define	DIV_ASSIGNMENT	303
#define	EXP_ASSIGNMENT	304
#define	BIT_AND_ASSIGNMENT	305
#define	DIV_NUM_ASSIGNMENT	306
#define	BIT_LEFT_SHIFT_ASSIGNMENT	307
#define	BIT_RIGHT_SHIFT_ASSIGNMENT	308
#define	UnarPlus	309
#define	UnarMinus	310
#define	MOD	311
#define	BIT_LEFT_SHIFT	312
#define	BIT_RIGHT_SHIFT	313
#define	NOT_EQUAL	314
#define	LESS_OR_SAME	315
#define	Is	316
#define	IsNot	317
#define	Like	318
#define	TypeOf	319
#define	Not	320
#define	MORE_OR_SAME	321
#define	OR	322
#define	ORELSE	323
#define	AND	324
#define	ANDALSO	325


extern YYSTYPE yylval;
