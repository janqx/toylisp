#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>
#include <ctype.h>
#include <assert.h>
#include <setjmp.h>

jmp_buf g_buf;

#define type(x) (x->type)
#define car(x) (x->v_cons.head)
#define cdr(x) (x->v_cons.tail)
#define cons(x, y) new_cons(x, y)
#define acons(x, y, z) cons(new_cons(x, y), z)
#define param1 car(x)
#define param2 car(cdr(x))
#define param3 car(cdr(cdr(x)))

#define INT_CACHE_MIN -128
#define INT_CACHE_MAX 128
#define INT_CACHE_NORMAL_INDEX(index) (index + (-1 * INT_CACHE_MIN))
#define TO_BOOL_OBJ(c) (c ? TrueObj : NilObj)
#define DEFINE_BUILTIN(name) static inline Obj* builtin_##name(Obj* env, Obj* x)
#define REF_COUNT(obj) (obj->ref_count)
#define DEC_REF(obj) if(--REF_COUNT(obj) < 1) { destroy_obj(obj); }
#define INC_REF(obj) (++REF_COUNT(obj))

typedef struct Obj Obj;
typedef enum ObjType ObjType;
typedef struct Parser Parser;
typedef Obj*(*Builtin)(Obj*, Obj*);

enum ObjType {
  T_NULL,
  T_BOOL,
  T_INT,
  T_FLOAT,
  T_STRING,
  T_SYMBOL,
  T_CONS,
  T_BUILTIN,
  T_LAMBDA,
  T_MACRO,
  T_ENV
};

struct Obj {
  ObjType type;
  int ref_count;
  union {
    int64_t v_int;
    double v_float;
    char* v_str;
    char* v_symbol;
    struct {
      Obj* head;
      Obj* tail;
    } v_cons;
    struct {
      Obj* name;
      int paramc;
      Builtin ptr;
      int ep;
    } v_builtin;
    struct {
      Obj* name;
      int paramc;
      Obj* params;
      Obj* body;
      Obj* env;
      Obj* rest;
    } v_lambda;
    struct {
      Obj* name;
      int paramc;
      Obj* params;
      Obj* body;
    } v_macro;
    struct {
      Obj* up;
      Obj* vars;
    } v_env;
  };
};

struct Parser {
  char* filename;
  char* source;
  int index;
  int length;
};

static Obj* NilObj;
static Obj* TrueObj;
static Obj* GlobalEnv;
static Obj* Symbols;
static Obj* IntCache[INT_CACHE_MAX - INT_CACHE_MIN + 1];

Obj* intern(const char* symbol);
Obj* parse(Parser* parser);
Obj* parse_obj(Parser* parser);
Obj* eval_list(Obj* env, Obj* x);
Obj* eval(Obj* env, Obj* x);
void add_var(Obj* env, Obj* symbol, Obj* obj);
Obj* find_var(Obj* env, Obj* symbol);
Obj* run(Obj* env, Obj* x);
const char* obj_to_str(Obj* x, char* str);

void print_stack_trace(Obj* env) {
  // TODO unimplements
}

void throw_error_v(Obj* env, const char* format, va_list ap) {
  print_stack_trace(env);
  vprintf(format, ap);
  printf("\n");
  longjmp(g_buf, 1);
}

void throw_error(Obj* env, const char* format, ...) {
  va_list ap;
  va_start(ap, format);
  throw_error_v(env, format, ap);
  va_end(ap);
}

void throw_error_assert(int cond, Obj* env, const char* format, ...) {
  if(cond) return;
  va_list ap;
  va_start(ap, format);
  throw_error_v(env, format, ap);
  va_end(ap);
}

int list_length(Obj* x) {
  if(x != NilObj && type(x) != T_CONS) return -1;
  int i = 0;
  for(Obj* p = x; p != NilObj; p = cdr(p), ++i) {
    if(type(p) != T_CONS) return -1;
  }
  return i;
}

void destroy_obj(Obj* obj) {
  assert(REF_COUNT(obj) == 0);
  free(obj);
}

Obj* new_obj(ObjType type) {
  Obj* obj = (Obj*)malloc(sizeof(Obj));
  obj->type = type;
  REF_COUNT(obj) = 0;
  return obj;
}

Obj* new_cons(Obj* head, Obj* tail) {
  Obj* obj = new_obj(T_CONS);
  obj->v_cons.head = head;
  obj->v_cons.tail = tail;
  return obj;
}

Obj* __new_int(int64_t val) {
  Obj* obj = new_obj(T_INT);
  obj->v_int = val;
  return obj;
}

Obj* new_int(int64_t val) {
  if(val >= INT_CACHE_MIN && val <= INT_CACHE_MAX) {
    return IntCache[INT_CACHE_NORMAL_INDEX(val)];
  }
  return __new_int(val);
}

Obj* new_float(double val) {
  Obj* obj = new_obj(T_FLOAT);
  obj->v_float = val;
  return obj;
}

Obj* new_string(const char* val) {
  Obj* obj = new_obj(T_STRING);
  obj->v_str = strdup(val);
  return obj;
}

Obj* new_symbol(const char* val) {
  Obj* obj = new_obj(T_SYMBOL);
  obj->v_symbol = strdup(val);
  Symbols = new_cons(obj, Symbols);
  return obj;
}

Obj* new_env(Obj* up, Obj* vars) {
  Obj* obj = new_obj(T_ENV);
  obj->v_env.up = up;
  obj->v_env.vars = vars;
  return obj;
}

Obj* push_env(Obj* env, Obj* vars, Obj* values, Obj* rest_param) {
  Obj* map = NilObj;
  for(Obj* p = vars, *q = values; p != NilObj; p = cdr(p), q = cdr(q)) {
    Obj* sym = car(p);
    Obj* val = car(q);
    if(rest_param != NilObj && sym == rest_param) {
      map = acons(sym, q, map);
      break;   
    } else {
      map = acons(sym, val, map);
    }
  }
  return new_env(env, map);
}

const char* obj_type_to_str(ObjType type) {
  switch(type) {
    case T_NULL: return "NULL";
    case T_BOOL: return "BOOL";
    case T_INT: return "INT";
    case T_FLOAT: return "FLOAT";
    case T_STRING: return "STRING";
    case T_SYMBOL: return "SYMBOL";
    case T_CONS: return "CONS";
    case T_BUILTIN: return "BUILTIN";
    case T_LAMBDA: return "LAMBDA";
    case T_MACRO: return "MACRO";
    case T_ENV: return "ENV";
    default: break;
  }
  return "UNKOWN_TYPE";
}

const char* obj_to_str(Obj* x, char* str) {
  switch(type(x)) {
    case T_NULL: strcpy(str, "NIL"); break;
    case T_BOOL: strcpy(str, "T"); break;
    case T_INT: sprintf(str, "%" PRId64, x->v_int); break;
    case T_FLOAT: sprintf(str, "%.16g", x->v_float); break;
    case T_STRING: strcpy(str, x->v_str); break;
    case T_SYMBOL: strcpy(str, x->v_symbol); break;
    case T_CONS: {
      char* ptr = str;
      *ptr++ = '(';
      for(Obj* p = x; p != NilObj; p = cdr(p)) {
        if(type(p) == T_CONS) {
          ptr += (int)strlen(obj_to_str(car(p), ptr));
          if(cdr(p) != NilObj) *ptr++ = ' ';
        } else {
          *ptr++ = '.';
          *ptr++ = ' ';
          ptr += (int)strlen(obj_to_str(p, ptr));
          break;
        }
      }
      *ptr++ = ')';
      *ptr = '\0';
      break;
    }
    case T_BUILTIN: {
      sprintf(str, "<BUILTIN %s(%d)>", x->v_builtin.name->v_symbol, x->v_builtin.paramc);      
      break;
    }
    case T_LAMBDA: {
      sprintf(str, "<LAMBDA %s(%d)>", x->v_lambda.name->v_symbol, x->v_lambda.paramc);      
      break;
    }
    case T_MACRO: {
      sprintf(str, "<MACRO %s(%d)>", x->v_macro.name->v_symbol, x->v_macro.paramc);      
      break;
    }
    default: {
      sprintf(str, "<%s 0x%p>", obj_type_to_str(type(x)), x);
      break;
    }
  }
  return str;
}

int peek_char(Parser* parser) {
  if(parser->index < parser->length) {
    return parser->source[parser->index];
  }
  return EOF;
}

int next_char(Parser* parser) {
  if(parser->index < parser->length) {
    return parser->source[parser->index++];
  }
  return EOF;
}

void skip_char(Parser* parser, int ch) {
  int c = next_char(parser);
  assert(c == ch);
}

void skip_whitespace(Parser* parser) {
  while(isspace(peek_char(parser))) {
    next_char(parser);
  }
}

char* read_file_to_text(const char* filename) {
  FILE* fp = fopen(filename, "r");
  if(!fp) {
    return NULL;
  }
  fseek(fp, 0, SEEK_END);
  long size = ftell(fp);
  fseek(fp, 0, SEEK_SET);
  char* buf = (char*)malloc(size + 1);
  fread(buf, 1, size, fp);
  fclose(fp);
  buf[size] = '\0';
  return buf;
}

Obj* parse_file(const char* filename) {
  Parser parser;
  char* source = read_file_to_text(filename);
  if(!source) {
    printf("can't open file: %s\n", filename);
    exit(-1);
    return NULL;
  }
  parser.filename = strdup(filename);
  parser.source = source;
  parser.index = 0;
  parser.length = strlen(parser.source);
  return parse(&parser);
}

Obj* parse_source(const char* source) {
  Parser parser;
  parser.filename = strdup("<STDIN>");
  parser.source = strdup(source);
  parser.index = 0;
  parser.length = strlen(parser.source);
  return parse(&parser);
}

Obj* parse(Parser* parser) {
  Obj *head, *tail;
  head = tail = cons(parse_obj(parser), NilObj);
  while(peek_char(parser) != EOF) {
    tail->v_cons.tail = cons(parse_obj(parser), NilObj);
    tail = tail->v_cons.tail;
  }
  return head;
}

Obj* parse_quote(Parser* parser) {
  skip_char(parser, '\'');
  return cons(intern("quote"), cons(parse_obj(parser), NilObj));
}

Obj* parse_list(Parser* parser) {
  skip_char(parser, '(');
  if(peek_char(parser) == ')') {
    next_char(parser);
    return NilObj;
  }
  Obj *head, *tail;
  head = tail = cons(parse_obj(parser), NilObj);
  while(peek_char(parser) != ')') {
    tail->v_cons.tail = cons(parse_obj(parser), NilObj);
    tail = tail->v_cons.tail;
    skip_whitespace(parser);
  }
  skip_char(parser, ')');
  return head;
}

Obj* parse_number(Parser* parser) {
  char str[32] = { 0 };
  char* ptr = str;
  int isfloat = 0;
  while(isdigit(peek_char(parser))) {
    *ptr++ = next_char(parser);
  }
  if(peek_char(parser) == '.') {
    isfloat = 1;
    *ptr++ = next_char(parser);
    if(!isdigit(peek_char(parser))) {
      printf("invalid syntax\n");
      exit(-1);
    }
    while(isdigit(peek_char(parser))) {
      *ptr++ = next_char(parser);
    }
  }
  *ptr = '\0';
  if(isfloat) {
    return new_float(atof(str));
  } else {
    return new_int(atoll(str));
  }
}

Obj* parse_string(Parser* parser) {
  skip_char(parser, '\"');
  char str[3096] = { 0 };
  char *p = str;
  while(peek_char(parser) != '\"') {
    char c = next_char(parser);
    if(c == '\\') {
      c = next_char(parser);
      switch(c) {
      case 'a':
        c = '\a';
        break;
      case 'b':
        c = '\b';
        break;
      case 'f':
        c = '\f';
        break;
      case 'n':
        c = '\n';
        break;
      case 't':
        c = '\t';
        break;
      case 'r':
        c = '\r';
        break;
      case 'v':
        c = '\v';
        break;
      case '\\':
        c = '\\';
        break;
      case '\"':
        c = '\"';
        break;
      default: break;
      }
    }
    *p++ = c;
  }
  skip_char(parser, '\"');
  *p = '\0';
  return new_string(str);
}

Obj* parse_symbol(Parser* parser) {
  char symbol[1024] = { 0 };
  char* ptr = symbol;
  *ptr++ = next_char(parser);
  while(1) {
    int c = peek_char(parser);
    if(isalpha(c) || strchr("_+-*/=!@#$%^&<>", c) || isdigit(c)) {
      *ptr++ = next_char(parser);
    } else {
      break;
    }
  }
  *ptr = '\0';
  return intern(symbol);
}

Obj* parse_obj(Parser* parser) {
  while(1) {
    int c = peek_char(parser);
    if(isspace(c)) {
      skip_whitespace(parser);
      continue;
    }
    if(c == EOF || c == '\0') {
      break;
    }
    if(c == ';') {
      while(peek_char(parser) != '\n') {
        next_char(parser);
      }
      skip_char(parser, '\n');
      continue;
    }
    if(c == '(') {
      return parse_list(parser);
    }
    if(isdigit(c)) {
      return parse_number(parser);
    }
    if(c == '\"') {
      return parse_string(parser);
    }
    if(c == '\'') {
      return parse_quote(parser);
    }
    if(isalpha(c) || strchr("_+-*/=!@#$%^&<>", c)) {
      return parse_symbol(parser);
    }
    throw_error(GlobalEnv, "ParserError: unprocessed character: %c", c);
  }
  return NilObj;
}

Obj* intern(const char* s) {
  char* symbol = strdup(s);
  symbol = strupr(symbol);
  for(Obj* p = Symbols; p != NilObj; p = cdr(p)) {
    if(strcmp(car(p)->v_symbol, symbol) == 0) {
      return car(p);
    }
  }
  Obj* obj = new_symbol(symbol);
  INC_REF(obj);
  free(symbol);
  return obj;
}

Obj* print(Obj* x) {
  char str[1024] = { 0 };
  printf("%s", obj_to_str(x, str));
  return NilObj;
}

Obj* progn(Obj* env, Obj* x) {
  Obj* res = NilObj;
  Obj* newEnv = new_env(env, NilObj);
  for(Obj* p = x; p != NilObj; p = cdr(p)) {
    res = eval(newEnv, car(p));
  }
  return res;
}

Obj* macroexpand(Obj* env, Obj* macro, Obj* args) {
  return progn(push_env(env, macro->v_macro.params, args, NilObj), macro->v_macro.body);
}

DEFINE_BUILTIN(print) {
  for(Obj* p = x; p != NilObj; p = cdr(p)) {
    print(car(p));
    printf(" ");
  }
  return NilObj;
}

DEFINE_BUILTIN(println) {
  builtin_print(env, x);
  putchar('\n');
  return NilObj;
}

DEFINE_BUILTIN(car) {
  return car(car(x));
}

DEFINE_BUILTIN(cdr) {
  return cdr(car(x));
}

DEFINE_BUILTIN(cons) {
  return cons(param1, param2);
}

DEFINE_BUILTIN(progn) {
  return progn(env, x);
}

DEFINE_BUILTIN(set) {
  throw_error_assert(type(param1) == T_SYMBOL, env, "can't set to type(%s)", obj_type_to_str(type(param1)));
  throw_error_assert(cdr(x) != NilObj, env, "can't set to too few arguments");
  Obj* var = find_var(env, param1);
  Obj* obj = eval(env, param2);
  INC_REF(obj);
  if(var != NilObj) {
    DEC_REF(cdr(var));
    cdr(var) = obj;
  } else {
    add_var(env, param1, obj);
  }
  if(cdr(cdr(x)) != NilObj) {
    builtin_set(env, cdr(cdr(x)));
  }
  return NilObj;
}

DEFINE_BUILTIN(lambda) {
  char name[64] = { 0 };
  Obj* params = param1;
  Obj* lambda = new_obj(T_LAMBDA);
  lambda->v_lambda.rest = NilObj;
  for(Obj* p = params; p != NilObj; p = cdr(p)) {
    if(car(p) != intern("&rest")) {
      continue;
    }
    if(cdr(p) == NilObj) {
      throw_error(env, "invalid syntax: next param of &rest is null");
    }
    if(cdr(cdr(p)) != NilObj) {
      throw_error(env, "invalid syntax: next param of &rest is not last param");
    }
    car(p) = car(cdr(p));
    cdr(p) = NilObj;
    lambda->v_lambda.rest = car(p);
    break;
  }
  sprintf(name, "<%p>", lambda);
  lambda->v_lambda.name = intern(name);
  lambda->v_lambda.paramc = list_length(params);
  lambda->v_lambda.params = params;
  lambda->v_lambda.body = cdr(x);
  lambda->v_lambda.env = env;
  return lambda;
}

DEFINE_BUILTIN(defmacro) {
  Obj* macro = new_obj(T_MACRO);
  macro->v_macro.name = param1;
  macro->v_macro.params = param2;
  macro->v_macro.paramc = list_length(param2);
  macro->v_macro.body = cdr(cdr(x));
  add_var(env, param1, macro);
  return macro;
}

DEFINE_BUILTIN(macroexpand) {
  return macroexpand(env, cdr(find_var(env, car(car(x)))), cdr(car(x)));
}

DEFINE_BUILTIN(quote) {
  return car(x);
}

DEFINE_BUILTIN(typeof) {
  return intern(obj_type_to_str(type(param1)));
}

#define binary_op(f, op) \
DEFINE_BUILTIN(f) {    \
  Obj* a = param1; \
  Obj* b = param2; \
  if(strcmp(#op, "+") == 0 && (type(a) == T_STRING && type(b) == T_STRING)) return new_string(strcat(a->v_str, b->v_str)); \
  if(type(a) == T_INT) {  \
    if(type(b) == T_INT) return new_int(a->v_int op b->v_int);    \
    else if(type(b) == T_FLOAT) return new_int((double)(a->v_int) op b->v_float);   \
  } else if(type(a) == T_FLOAT) { \
    if(type(b) == T_INT) return new_float(a->v_float op (double)(b->v_int));   \
    else if(type(b) == T_FLOAT) return new_float(a->v_float op b->v_float);  \
  } \
  throw_error(env, "TypeError: unsupported operand type(s) for %s: '%s' and '%s'", #op, obj_type_to_str(type(a)), obj_type_to_str(type(b))); \
  return NilObj; \
}

binary_op(add, +);
binary_op(sub, -);
binary_op(mul, *);
binary_op(div, /);

#define binary_logic_op(f, op) \
DEFINE_BUILTIN(f) { \
  Obj* a = param1; \
  Obj* b = param2; \
  if((a == b) && (strcmp(#op , "==") == 0)) return TrueObj; \
  if((a == NilObj || b == NilObj) && (strcmp(#op , "==") == 0 || strcmp(#op , "!=") == 0)) return TO_BOOL_OBJ(a op b); \
  if(type(a) == T_INT) { \
    if(type(b) == T_INT) return TO_BOOL_OBJ(a->v_int op b->v_int); \
    if(type(b) == T_FLOAT) return TO_BOOL_OBJ(a->v_int op (int64_t)(b->v_float)); \
  } \
  if(type(a) == T_FLOAT) { \
    if(type(b) == T_FLOAT) return TO_BOOL_OBJ(a->v_float op b->v_float); \
    if(type(b) == T_INT) return TO_BOOL_OBJ(a->v_float op (double)(b->v_int)); \
  } \
  if(type(a) == T_STRING && type(b) == T_STRING && strcmp(#op , "==") == 0) return TO_BOOL_OBJ(strcmp(a->v_str, b->v_str) == 0); \
  if(type(a) == T_STRING && type(b) == T_STRING && strcmp(#op , "!=") == 0) return TO_BOOL_OBJ(strcmp(a->v_str, b->v_str) != 0); \
  if(type(a) == T_SYMBOL && type(b) == T_SYMBOL && (strcmp(#op , "==") == 0 || strcmp(#op , "!=") == 0)) return TO_BOOL_OBJ(a op b); \
  throw_error(env, "TypeError: unsupported operand type(s) for %s: '%s' and '%s'", #op, obj_type_to_str(type(a)), obj_type_to_str(type(b))); \
  return NilObj; \
} \

binary_logic_op(eq, ==);
binary_logic_op(neq, !=);
binary_logic_op(gt, >);
binary_logic_op(gte, >=);
binary_logic_op(lt, <);
binary_logic_op(lte, <=);

DEFINE_BUILTIN(cond) {
  for(Obj* p = x; p != NilObj; p = cdr(p)) {
    Obj* item = car(p);
    Obj* cond = eval(env, car(item));
    if(cond != NilObj) {
      return eval(env, car(cdr(item)));
    }
  }
  return NilObj;
}

DEFINE_BUILTIN(while) {
  while(eval(env, car(x)) != NilObj) {
    eval(env, car(cdr(x)));
  }
  return NilObj;
}

DEFINE_BUILTIN(eval) {
  if(type(param1) == T_STRING) {
    return run(env, parse_source(param1->v_str));
  }
  return eval(env, param1);
}

void add_var(Obj* env, Obj* symbol, Obj* obj) {
  env->v_env.vars = acons(symbol, obj, env->v_env.vars);
}

Obj* find_var(Obj* env, Obj* symbol) {
  Obj* vars = env->v_env.vars;
  if(vars != NilObj) {
    for(Obj* var = car(vars); vars != NilObj; vars = cdr(vars), var = car(vars)) {
      if(car(var) == symbol || strcmp(car(var)->v_symbol, symbol->v_symbol) == 0) {
        return var;
      }
    }
  }
  if(env->v_env.up != NilObj) {
    return find_var(env->v_env.up, symbol);
  }
  return NilObj;
}

void init_global_vars() {
  NilObj = new_obj(T_NULL);
  TrueObj = new_obj(T_BOOL);
  GlobalEnv = new_env(NilObj, NilObj);
  Symbols = NilObj;
  add_var(GlobalEnv, intern("NIL"), NilObj);
  add_var(GlobalEnv, intern("T"), TrueObj);
  INC_REF(NilObj);
  INC_REF(TrueObj);
}

void add_builtin(Obj* env, const char* name, Builtin builtin, int paramc, int ep) {
  Obj* obj = new_obj(T_BUILTIN);
  obj->v_builtin.name = intern(name);
  obj->v_builtin.paramc = paramc;
  obj->v_builtin.ptr = builtin;
  obj->v_builtin.ep = ep;
  INC_REF(obj);
  add_var(env, obj->v_builtin.name, obj);
}

void init_builtins(Obj* env) {
  add_builtin(GlobalEnv, "print", builtin_print, -1, 1);
  add_builtin(GlobalEnv, "println", builtin_println, -1, 1);
  add_builtin(GlobalEnv, "car", builtin_car, 1, 1);
  add_builtin(GlobalEnv, "cdr", builtin_cdr, 1, 1);
  add_builtin(GlobalEnv, "cons", builtin_cons, 2, 1);
  add_builtin(GlobalEnv, "progn", builtin_progn, -1, 0);
  add_builtin(GlobalEnv, "set", builtin_set, -1, 0);
  add_builtin(GlobalEnv, "lambda", builtin_lambda, 2, 0);
  add_builtin(GlobalEnv, "defmacro", builtin_defmacro, 3, 0);
  add_builtin(GlobalEnv, "macroexpand", builtin_macroexpand, 1, 1);
  add_builtin(GlobalEnv, "quote", builtin_quote, 1, 0);
  add_builtin(GlobalEnv, "typeof", builtin_typeof, 1, 1);
  add_builtin(GlobalEnv, "+", builtin_add, 2, 1);
  add_builtin(GlobalEnv, "-", builtin_sub, 2, 1);
  add_builtin(GlobalEnv, "*", builtin_mul, 2, 1);
  add_builtin(GlobalEnv, "/", builtin_div, 2, 1);
  add_builtin(GlobalEnv, "==", builtin_eq, 2, 1);
  add_builtin(GlobalEnv, "!=", builtin_neq, 2, 1);
  add_builtin(GlobalEnv, ">", builtin_gt, 2, 1);
  add_builtin(GlobalEnv, ">=", builtin_gte, 2, 1);
  add_builtin(GlobalEnv, "<", builtin_lt, 2, 1);
  add_builtin(GlobalEnv, "<=", builtin_lte, 2, 1);
  add_builtin(GlobalEnv, "cond", builtin_cond, -1, 0);
  add_builtin(GlobalEnv, "while", builtin_while, 2, 0);
  add_builtin(GlobalEnv, "eval", builtin_eval, 1, 1);
}

Obj* call(Obj* env, Obj* callable, Obj* args) {
  int paramc = -1;
  int argc = list_length(args);
  Obj* name = NilObj;
  int is_rest = 0;
  if(type(callable) == T_BUILTIN) {
    paramc = callable->v_builtin.paramc;
    name = callable->v_builtin.name;
  }
  if(type(callable) == T_LAMBDA) {
    paramc = callable->v_lambda.paramc;
    name = callable->v_lambda.name;
    is_rest = callable->v_lambda.rest != NilObj;
  }
  if(type(callable) == T_MACRO) {
    paramc = callable->v_macro.paramc;
    name = callable->v_macro.name;
  }
  if(is_rest) {
    throw_error_assert(argc >= paramc - 1, env, 
    "%s() the number of arguments is less than %d", 
    name->v_symbol, paramc - 1);
  } else {
    throw_error_assert(paramc == -1 || argc == paramc, env, 
    "%s() takes %d positional arguments but %d were given", 
    name->v_symbol, paramc, argc);
  }
  if(type(callable) == T_MACRO) {
    return eval(env, macroexpand(env, callable, args));
  }
  if(type(callable) == T_LAMBDA || (type(callable) == T_BUILTIN && callable->v_builtin.ep)) {
    args = eval_list(env, args);
  }
  if(type(callable) == T_BUILTIN) {
    return callable->v_builtin.ptr(env, args);
  }
  if(type(callable) == T_LAMBDA) {
    Obj* newEnv = push_env(callable->v_lambda.env, callable->v_lambda.params, args, callable->v_lambda.rest);
    return builtin_progn(newEnv, callable->v_lambda.body);
  }
  char objStr[1024] = { 0 };
  throw_error(env, "can't call type: %s(%s)", obj_type_to_str(type(callable)), obj_to_str(callable, objStr));
  return NilObj;
}

Obj* eval_list(Obj* env, Obj* x) {
  Obj *head, *tail;
  head = tail = NULL;
  for(Obj *p = x; p != NilObj; p = cdr(p)) {
    Obj *tmp = eval(env, car(p));
    if(head == NULL) {
      head = tail = cons(tmp, NilObj);
    } else {
      tail->v_cons.tail = cons(tmp, NilObj);
      tail = tail->v_cons.tail;
    }
  }
  return head == NULL ? NilObj : head;
}

Obj* eval(Obj* env, Obj* x) {
  switch(type(x)) {
    case T_NULL:
    case T_BOOL:
    case T_INT:
    case T_FLOAT:
    case T_STRING:
      return x;
    case T_SYMBOL: {
      Obj* obj = find_var(env, x);
      if(obj == NilObj) {
        throw_error(env, "can't find symbol: %s", x->v_symbol);
      }
      return cdr(obj);
    }
    case T_CONS: {
      return call(env, eval(env, car(x)), cdr(x));
    }
    default: break;
  }
  return x;
}

Obj* run(Obj* env, Obj* x) {
  Obj* res = NilObj;
  int code = setjmp(g_buf);
  if(code == 0) {
    for(Obj* p = x; p != NilObj; p = cdr(p)) {
      res = eval(env, car(p));
    }
    return res;
  } else {
    // catch exception...
  }
  return NilObj;
}

void init_int_cache() {
  for(int i = INT_CACHE_MIN; i <= INT_CACHE_MAX; i++) {
    IntCache[INT_CACHE_NORMAL_INDEX(i)] = __new_int((int64_t)i);
    REF_COUNT(IntCache[INT_CACHE_NORMAL_INDEX(i)]) = 1;
  }
}

void init() {
  init_global_vars();
  init_builtins(GlobalEnv);
  init_int_cache();
}

void repl() {
  char input[1024];
  char* ptr = NULL;
  int ch;
  for(;;) {
    ptr = input;
    printf(">>> ");
    while((ch = getchar()) != EOF && ch != '\n') {
      *ptr++ = ch;
    }
    *ptr = '\0';
    print(run(GlobalEnv, parse_source(input)));
    printf("\r\n");
  }
}

int main(int argc, char const *argv[]) {
  init();
  run(GlobalEnv, parse_file("./lib.lisp"));
  if(argc > 1) {
    print(run(GlobalEnv, parse_file(argv[1])));
  } else {
    repl();
  }
  return 0;
}
