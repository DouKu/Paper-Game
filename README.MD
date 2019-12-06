### 一个我小时候玩过的游戏

#### 目录
- [游戏简介](#游戏简介)
- [玩法规则](https://github.com/DouKu/Paper-Game/blob/master/doc/play_rule/PlayRules.md) 
- [编译](https://github.com/DouKu/Paper-Game/blob/master/doc/make/Make.md) 
- [Proto](https://github.com/DouKu/Paper-Game/blob/master/doc/proto/Proto.md) 
- [Base engine](https://github.com/DouKu/Paper-Game/blob/master/doc/engine/Engine.md) 

#### TODO
- [ ] role process
- [ ] battle system

#### 游戏简介
- 类型：回合制
- 利用可胜利的游戏，进行地图战
- 主要流程：
    1. 参战双方进入场地。场地为长方形并且双方各有一个基地。
    2. 参战双方玩石头剪刀布（可用另外的短小游戏代替）
    3. 胜者可以进行一次行动
    4. 进行一次行动，包括如下：
        1. 建造
        2. 保存胜利次数
        3. 攻击指令
    5. 执行完后有一方基地被攻破，游戏结束。否则重新执行2-5
- 游戏图片:
![f_57316361.png](http://newtc.dajiqq.com/data/f_57316361.png)