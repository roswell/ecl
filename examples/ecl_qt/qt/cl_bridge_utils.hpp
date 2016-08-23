#ifndef CL_BRIDGE_UTILS_HPP
#define CL_BRIDGE_UTILS_HPP
#include <iostream>
#include <string>
#ifdef slots
#undef slots
#endif
#include <ecl/ecl.h>

using std::string;
using lisp_expr = std::string;
using std::cout;
using std::endl;
//extern string  CL_MAIN_FASB ;
//extern string  CL_MAIN_PACKAGE_NAME ;

cl_object lispfy(string str); /* convert a std::string to cl_object */



/* add spaces among several strings. */
string __spc_expr(string first);
template <typename ...str>
string __spc_expr (string first, str ... next){
    return first+" "+__spc_expr(next...);
}

/* encapsule expressions in parenthesis. */
/* to create lisp expr. */
template<typename ...str>
lisp_expr par_expr(str... all){
    return "("+__spc_expr(all...)+")";
}

/* turn the sequence into a lisp list expr. */
/* ex: par_list("hello", "lisp", "world");
 * -> '("hello" "lisp" "world") */
template<typename ...str>
lisp_expr par_list(str... all){
    return "'"+par_expr(all...);
}

/* an enhanced version of cl_eval */
template<typename ...str>
cl_object cl_eval(str... all){
    std::cout<<par_expr(all...)<<std::endl;
    return cl_eval(lispfy(par_expr(all...)));
}

auto cl_list_traverse=[](auto& cl_lst, auto fn){
    while(!Null(cl_lst))
    {
        fn(cl_car(cl_lst));
        cl_lst=cl_cdr(cl_lst);
    }
};


/* enhanced cl_object in c++ */
class cl_obj {
private:
    cl_object __obj;
public:

    cl_obj(cl_object &&obj){this->__obj=obj;}
    cl_obj(const cl_object &obj){this->__obj=obj;}

    /* list index */
    inline cl_obj car(){return cl_obj(cl_car(this->__obj));}
    inline cl_obj cdr(){return cl_obj(cl_cdr(this->__obj));}
    inline cl_obj cadr(){return this->cdr().car();}
    inline cl_obj caar(){return this->car().car();}
    inline cl_obj cddr(){return this->cdr().cdr();}

    /* predicates */
    inline bool nullp(){return Null(this->__obj);}
    inline bool atomp(){return ECL_ATOM(this->__obj);}
    inline bool listp(){return ECL_LISTP(this->__obj);}
    inline bool symbolp(){return ECL_SYMBOLP(this->__obj);}

    inline int to_int(){return ecl_to_int(this->__obj);}
    inline char to_char(){return ecl_to_char(this->__obj);}

    /* turn the cl_object into string. */
    inline std::string to_std_string(){
        std::string val;
        auto & str=this->__obj->string;
        for(unsigned long i=0;i<str.fillp;i+=1)
           val+=*(typeof(str.elttype) *)(str.self+i);
        return val;
    }

    /* traverse the cl_object as a list. */
    template<typename function>
    inline void list_traverse(function fn){cl_list_traverse(this->__obj, fn);}

    inline cl_obj operator=(cl_object &&obj){return cl_obj(obj);}

};



#endif // CL_BRIDGE_UTILS_HPP
