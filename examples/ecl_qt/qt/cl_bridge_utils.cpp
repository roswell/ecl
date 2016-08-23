#include "cl_bridge_utils.hpp"


cl_object lispfy(string str){
  return c_string_to_object(str.data());
}

string __spc_expr(string first){
  return first;
}

