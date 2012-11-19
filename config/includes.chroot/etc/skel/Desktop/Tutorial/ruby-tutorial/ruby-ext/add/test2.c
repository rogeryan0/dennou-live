#include "ruby.h"

int add(int a, int b);

VALUE wrap_add(self, aa, bb)
     VALUE self, aa, bb;
{
  int a, b, result;
  
  a = FIX2INT(aa);
  b = FIX2INT(bb);
  result = add(a,b);
  return INT2FIX(result);
}

void Init_test()
{
  VALUE module;

  module = rb_define_module("Test");
  rb_define_module_function(module, "add", wrap_add, 2);
}
