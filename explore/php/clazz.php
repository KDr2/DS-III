<?php

trait BaseTrait
{
    public function tf_0(){
        echo "----> enter tf_0\n";
        print_r($this);
        print_r(get_class($this));
        echo "\n";
        echo "<---- exit tf_0\n";
    }
}

class Base
{
    public static $s_var_0 = 1;
    public $m_var_0 = 1;

    use BaseTrait;

    public function foo_0(){
        echo "---> enter foo_0\n";
        print_r($this);
        // class name
        print_r(get_class($this));
        echo "\n";
        print_r(get_called_class());
        echo "\n";
        echo "foo_0 in Base\n";
        echo "m_var_0: ". $this->m_var_0 . "\n";
        echo "<--- exit foo_0\n";
    }

    public static function bar_0(){
        echo "---> enter bar_0\n";
        print_r($this);
        // class name
        print_r(get_called_class());
        echo "\n";
        echo "bar_0 in Base\n";
        echo "<--- exit bar_0\n";
    }
}

// get properties
$r = new ReflectionClass('Base');

print_r($r->getProperties());
print_r($r->getStaticProperties());
print_r($r->getStaticPropertyValue('s_var_0')."\n");

// call function
$obj = new Base();
$obj->tf_0();
//call_user_func(array('Base', 'foo_0'));
call_user_func(array($obj, 'foo_0'));


call_user_func(array('Base', 'bar_0'));
call_user_func(array($obj, 'bar_0'));
