
/*
 * rbtree usage example
 * author : KDr2
 *
 */



#include <stdlib.h>
#include <stdio.h>

#include "include/rbtree_example.h"

struct test_node{
    int value;
    struct rb_node node;
};


struct test_node *create_node(int v){
    struct test_node *ret=(struct test_node*)malloc(sizeof(struct test_node));
    ret->value=v;
    RB_CLEAR_NODE(&(ret->node));
    ret->node.rb_left=NULL;
    ret->node.rb_right=NULL;
    return ret;
}

void print_node(struct test_node *n){
    printf("============\n");
    printf("Node Value: %d\n",n->value);
    printf("Node Color: %s\n",rb_color(&n->node)?"Black":"Red");
    printf("------------\n");
}


struct test_node *find_node(struct rb_root *root, int val){
    struct rb_node *node=root->rb_node;
    while(node){
        struct test_node *e=rb_entry(node,struct test_node,node);
        if(val > e->value){
            node=node->rb_right;
        }else if(val < e->value){
            node=node->rb_left;
        }else{
            return e;
        }
    }
    return NULL;
}

struct test_node *erase_node(struct rb_root *root, int val){
    struct test_node *target=find_node(root,val);
    if(target){
        rb_erase(&target->node,root);
    }
    return target;
}

struct test_node *insert_node(struct rb_root *root, struct test_node *n){
    struct rb_node **p=&root->rb_node;
    struct rb_node *parent=NULL;
    while(*p){
        parent=*p;
        struct test_node *vnode=rb_entry(parent,struct test_node,node);
        int v=vnode->value;
        if(v > n->value){
            p=&parent->rb_left;
        }else if(v < n->value){
            p=&parent->rb_right;
        }else{
            return vnode;
        }
    }
    rb_link_node(&n->node,parent,p);
    rb_insert_color(&(n->node),root);
    return n;
}


int rbtree_test(){
    struct rb_root root=RB_ROOT;
    struct test_node *tmp;

    int i=10;
    for(;i>0;i--){
        tmp=create_node(i);
        printf("+++ i=%d\n",i);
        insert_node(&root,tmp);
        print_node(tmp);
    }

    struct test_node *r=erase_node(&root,7);
    free(r);

    while(rb_parent(&tmp->node)){
        tmp=rb_entry(rb_parent(&tmp->node),struct test_node,node);
    }

    print_node(tmp);
    print_node(rb_entry(root.rb_node,struct test_node,node));

    return 0;
}


