### 一个石头剪刀布的游戏

#### 目录
* [游戏主流程](#游戏主流程)
* [攻击建造](https://github.com/QiIL/Paper-Game/)
* [防御建造](https://github.com/QiIL/Paper-Game/)
* [攻击指令](#攻击指令)

#### 游戏主流程:
1. 参战双方进入场地。场地为长方形并且双方各有一个基地。
2. 参战双方玩石头剪刀布（可用另外的短小游戏代替）
3. 胜者可以进行一次行动
4. 进行一次行动，包括如下：
    1. 建造
    2. 保存胜利次数
    3. 攻击指令
5. 执行完后有一方基地被攻破，游戏结束。否则重新执行2-5

#### 攻击建造
- 炮台：
    - 成本：1次胜利次数
    - 攻击力: 1
    - 效果：普普通通的可以一直使用的炮台

#### 防御建造
- 防护罩
    - 成本: 1次攻击次数
    - 效果：抵挡1点攻击力后消失

#### 攻击指令
- 让所有可以攻击的建造一起攻击