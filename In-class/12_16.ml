#include <stdio.h>
#include <stdlib.h>


enum tag {add, mul, val, neg} ;

struct expr ;


struct bin_op_struct {
    struct expr *l ;
    struct expr *r ;
} ;


union all_components { 
    struct bin_op_struct add_components ;
    struct bin_op_struct mul_components ;
    int v ;
    struct expr *ne ;
} ; 

struct expr {
    enum tag expr_tag ;
    union all_components components ;
};

int eval (struct expr *e) {
    switch (e->expr_tag) {
    case add: {
        return ( eval (e->components.add_components.l) +
                 eval (e->components.add_components.r) ) ;
    }
    case mul: {
        return ( eval (e->components.mul_components.l) *
                 eval (e->components.mul_components.r) ) ;
    }
    case val: {
        return ( e->components.v ) ;
    }
    case neg: {
        return ( 0 - eval (e->components.ne) ) ;
    }
    }

}

int main () {

  

    struct expr *val_4 = malloc (sizeof (struct expr));
    val_4->expr_tag = val ;
    val_4->components.v = 4 ;

    struct expr *val_2 = malloc (sizeof (struct expr));
    val_2->expr_tag = val ;
    val_2->components.v = 2 ;

    struct expr *val_3 = malloc (sizeof (struct expr));
    val_3->expr_tag = val ;
    val_3->components.v = 3 ;

    struct expr *mul_expr = malloc (sizeof (struct expr));
    mul_expr->expr_tag = mul ;
    mul_expr->components.mul_components.l = val_2 ;
    mul_expr->components.mul_components.r = val_3 ;

    struct expr *add_expr = malloc (sizeof (struct expr));
    add_expr->expr_tag = add ;
    add_expr->components.add_components.l = val_4 ;
    add_expr->components.add_components.r = mul_expr ;


   
    printf ("Result is %d.\n", eval (add_expr) ) ;

}