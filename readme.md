## 介绍
通过写一个2048小游戏学习Haskell

## 思路
###### 0、自顶向下写法、从main开始组装整个巨函数
###### 1、main函数作为整个程序的入口，传入一个初始化数组，程序开始运行
###### 2、判断整个格点是否可以继续上下左右，如果不行则Game Over
###### 3、可以上下左右时，先判断是否胜利，未胜利的话刷新一下页面重新拉起main函数

## 总结
###### 1、函数式编程中的等于号是数学等于号，赋值号为<-
###### 2、万物皆是函数、代码中所有可见项均是函数
###### 3、没有for循环的概念，使用递归实现循环