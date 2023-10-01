%option noyywrap
%option never-interactive

DIGIT [0-9]

%x CHAR
%x STRING
%x ML_COMMENT

%{
	#include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include <ctype.h>

    char str[1024];
    int lineNumber = 1;
%}

%%

%{
    void removeUnderline();
    void removeType(char *typeName);
	void convertCharacterCodeToString(char characterCode[], int startPosition, char *string);
%}

\n { lineNumber++; }
" " { }

as { printf("Found KW_AS: %s\n", yytext); }
and { printf("Found KW_AND: %s\n", yytext); }
binary { printf("Found KW_BINARY: %s\n", yytext); }
ByRef { printf("Found KW_BYREF: %s\n", yytext); }
ByVal { printf("Found KW_BYVAL: %s\n", yytext); }
Date { printf("Found KW_DATE: %s\n", yytext); }
Do { printf("Found KW_DO: %s\n", yytext); }
else { printf("Found KW_ELSE: %s\n", yytext); }
enum { printf("Found KW_ENUM: %s\n", yytext); }
Empty { printf("Found KW_EMPTY: %s\n", yytext); }
False { printf("Found KW_FALSE: %s\n", yytext); }
Error { printf("Found KW_ERROR: %s\n", yytext); }
for { printf("Found KW_FOR: %s\n", yytext); }
if { printf("Found KW_IF: %s\n", yytext); }
Friend { printf("Found KW_FRIEND: %s\n", yytext); }
Get { printf("Found KW_GET: %s\n", yytext); }
let { printf("Found KW_LET: %s\n", yytext); }
Is { printf("Found KW_IS: %s\n", yytext); }
Len { printf("Found KW_LEN: %s\n", yytext); }
Lock { printf("Found KW_Lock: %s\n", yytext); }
Me { printf("Found KW_ME: %s\n", yytext); }
Mid { printf("Found KW_MID: %s\n", yytext); }
New { printf("Found KW_NEW: %s\n", yytext); }
Next { printf("Found KW_NEXT: %s\n", yytext); }
null { printf("Found KW_NULL: %s\n", yytext); }
return { printf("Found KW_RETURN: %s\n", yytext); }
On { printf("Found KW_ON: %s\n", yytext); }
Option { printf("Found KW_OPTION: %s\n", yytext); }
Optional { printf("Found KW_OPTIONAL: %s\n", yytext); }
Or { printf("Found KW_OR: %s\n", yytext); }
ParamArray { printf("Found KW_PARAMARRAY: %s\n", yytext); }
Print { printf("Found KW_PRINT: %s\n", yytext); }
Private { printf("Found KW_PRIVATE: %s\n", yytext); }
Protected { printf("Found KW_PROTECTED: %s\n", yytext); }
Property { printf("Found KW_PROPERTY: %s\n", yytext); }
PtrSafe { printf("Found KW_PTRSAFE: %s\n", yytext); }
Public { printf("Found KW_PUBLIC: %s\n", yytext); }
Resume { printf("Found KW_RESUME: %s\n", yytext); }
Seek { printf("Found KW_SEEK: %s\n", yytext); }
Set { printf("Found KW_SET: %s\n", yytext); }
Static { printf("Found KW_STATIC: %s\n", yytext); }
True  { printf("Found KW_TRUE: %s\n", yytext); }
Step { printf("Found KW_STEP: %s\n", yytext); }
String { printf("Found KW_STRING: %s\n", yytext); }
Then { printf("Found KW_THEN: %s\n", yytext); }
Time { printf("Found KW_TIME: %s\n", yytext); }
To { printf("Found KW_TO: %s\n", yytext); }
While { printf("Found KW_WHILE: %s\n", yytext); }
Where { printf("Found KW_WHERE: %s\n", yytext); }
Loop { printf("Found KW_LOOP: %s\n", yytext); }
Integer { printf("Found TYPE_INTEGER: %s\n", yytext); }
Char { printf("Found TYPE_CHAR: %s\n", yytext); }
String { printf("Found TYPE_STRING: %s\n", yytext); }

"+" { printf("Found OPERATOR_PLUS: %s\n", yytext); }
"-" { printf("Found OPERATOR_MINUS: %s\n", yytext); }
"*" { printf("Found OPERATOR_MUL: %s\n", yytext); }
"/" { printf("Found OPERATOR_DIV: %s\n", yytext); }

"=" { printf("Found OPERATOR_ASSIGNMENT: %s\n", yytext); }
"==" { printf("Found OPERATOR_EQUAL: %s\n", yytext); }
"<>" { printf("Found OPERATOR_NOT_EQUAL: %s\n", yytext); }
"<" { printf("Found OPERATOR_LESS: %s\n", yytext); }
">" { printf("Found OPERATOR_MORE: %s\n", yytext); }
"<=" { printf("Found OPERATOR_LESS_OR_EQUAL: %s\n", yytext); }
">=" { printf("Found OPERATOR_MORE_OR_EQUAL: %s\n", yytext); }

"(" { printf("Found LEFT_BRACKET: %s\n", yytext); }
")" { printf("Found RIGHT_BRACKET: %s\n", yytext); }