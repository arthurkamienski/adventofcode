import re

class Unit:
    def __init__(self):
        pass

class Group:
    def __init__(self, definition):
        self.parse_def(definition)
    
    def parse_def(self, d):
        group_re = r'(\d+).*?(\d+) hit points (\(.*?\))?.*?(\d+)\ (\w+).*?(\d+)'

        d = d.strip()
        d = re.search(group_re, d).groups()

        us, hps, parenthesis, dmg, dmg_type, init = d

        if parenthesis is not None:
            immune_re = r'immune to (.*?)[;)]'
            weak_re   = r'weak to (.*?)[;)]'
            
            if immune := re.search(immune_re, parenthesis):
                immune = immune.group(1).split(', ')
            else:
                immune = []

            if weak := re.search(weak_re, parenthesis):
                weak = weak.group(1).split(', ')
            else:
                weak = []
        else:
            weak   = []
            immune = []

        self.units  = int(us)
        self.hp     = int(hps)
        self.immns  = set(immune)
        self.weaks  = set(weak)
        self.dmg    = int(dmg)
        self.dmg_tp = dmg_type
        self.init   = int(init)

    def eff_pow(self):
        return self.units*self.dmg

    def dmg_to(self, other):
        dmg = 0
        if self.dmg_tp not in other.immns:
            dmg = self.eff_pow()
            if self.dmg_tp in other.weaks:
                dmg *= 2
        return dmg

    def choose_target(self, groups):
        if len(groups) > 0:
            groups = sorted(
                groups,
                key=lambda g: (
                    self.dmg_to(g),
                    g.eff_pow(),
                    g.init))
            if len([g for g in groups if self.dmg_to(g) != 0]) > 0:
                self.target = groups.pop()
            else:
                self.target = None
        else:
            self.target = None
        return groups

    def suffer_dmg(self, dmg):
        bef = self.units
        total_health = max(self.units*self.hp-dmg, 0)
        self.units = total_health // self.hp

        if total_health % self.hp != 0:
            self.units += 1
        
    def __repr__(self):
        return f'Group({self.units} units, {self.hp} hp, {self.dmg} dmg, {self.init} init)'

class Army:
    def __init__(self, d):
        self.d = d.strip().split('\n')[1:]
        self.reset()

    def reset(self):
        self.groups = [Group(g) for g in self.d]

    def selection_order(self):
        return sorted(self.groups, key=lambda g: (-g.eff_pow(), -g.init))

    def choose_targets(self, opponent):
        groups = opponent.groups

        for g in self.selection_order():
            groups = g.choose_target(groups)

    def clean_battlefield(self):
        self.groups = [g for g in self.groups if g.units != 0]

    def units(self):
        return sum(g.units for g in self.groups)

    def boost(self, b):
        for g in self.groups:
            g.dmg += b

def read_input(path):
    with open(path) as f:
        c = f.read()
        
    s, i = c.split('\n\n')
    s = Army(s)
    i = Army(i)

    return s, i

def battle(s, i, b=0):
    def target_selection():
        s.choose_targets(i)
        i.choose_targets(s)

    def attack():
        all_groups = s.groups + i.groups
        all_groups = sorted(all_groups, key=lambda g: -g.init)

        for g in all_groups:
            if g.target is not None:
                #print(f'{g} attacks {g.target}')
                g.target.suffer_dmg(g.dmg_to(g.target))
    s.reset()
    i.reset()
    s.boost(b)
    
    units = None

    while len(s.groups) != 0 and len(i.groups) != 0 and s.units() + i.units() != units:
        units = s.units() + i.units()

        target_selection()
        attack()
        s.clean_battlefield()
        i.clean_battlefield()


    return s.units(), i.units()

if __name__ == '__main__':
    s, i = read_input('input.txt')

    s_win, i_win = battle(s, i)

    print(s_win + i_win)

    boost = 1

    while s_win == 0 or i_win != 0:
        if boost % 100 == 0:
            print(boost)
        s_win, i_win = battle(s, i, boost)
        boost += 1

    print(s_win)
