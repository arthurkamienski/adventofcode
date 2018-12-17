import random

class Track:
    def __init__(self, char, cart=None):
        self.char = char
        self.cart = cart

    def get_char(self):
        if self.cart is None:
            char = self.char
        else:
            char = self.cart.get_char()

        return char

class Cart:
    def __init__(self, i, j, char):
        self.i = i
        self.j = j
        self.turn_num = 0

        self.speed_x = 0
        self.speed_y = 0

        if char == '^':
            self.speed_y = -1
        elif char == 'v':
            self.speed_y = 1
        elif char == '<':
            self.speed_x = -1
        elif char == '>':
            self.speed_x = 1

    def crossing(self):
        if self.turn_num == 0:
            self.turn('left')
        elif self.turn_num == 2:
            self.turn('right')
            self.turn_num = -1

        self.turn_num = self.turn_num+1

    def turn(self, direction):
        x = self.speed_x
        
        if direction == 'right':
            self.speed_x = -self.speed_y
            self.speed_y = x
        else:
            self.speed_x = self.speed_y
            self.speed_y = -x

    def get_char(self):
        if self.speed_x == 1:
            char = '>'
        elif self.speed_x == -1:
            char = '<'
        elif self.speed_y == -1:
            char = '^'
        else:
            char = 'v'

        return char

    def move(self):
        self.i = self.i + self.speed_y
        self.j = self.j + self.speed_x

class Track_Map:
    def __init__(self, raw_data):
        self.carts = []

        self.build_layout(raw_data)

    def build_layout(self, raw_data):
        self.layout = {}

        for i, line in enumerate(raw_data):
            self.layout[i] = {}
            for j, char in enumerate(line):
                if char not in ' \n':
                    if char in '><v^':
                        cart = Cart(i, j, char)
                        self.carts.append(Cart(i, j, char))

                        if char in 'v^':
                            self.layout[i][j] = Track('|', cart)
                        else:
                            self.layout[i][j] = Track('-', cart)
                    else:
                        self.layout[i][j] = Track(char)

    def print_map(self):
        for i, line in self.layout.items():
            k = 0
            for j, char in line.items():
                while k < j:
                    print(' ', end='')
                    k = k + 1
                print(self.layout[i][j].get_char(), end='')
                k = k + 1
            print()

    def print_cart_positions(self):
        print([(c.i, c.j) for c in self.carts])

    def has_collision(self, cart):
        return self.cart_track(cart).cart is not None

    def cart_track(self, cart):
        return self.layout[cart.i][cart.j]

    def turn_cart(self, cart):
        track = self.cart_track(cart).get_char()

        if track == '\\':
            if cart.get_char() in '^v':
                cart.turn('left')
            else:
                cart.turn('right')
        elif track == '/':
            if cart.get_char() in '^v':
                cart.turn('right')
            else:
                cart.turn('left')
        elif track == '+':
            cart.crossing()

    def tick(self):
        collision = None

        removed = []
        
        self.carts.sort(key=lambda x: (x.j, x.i))
        
        for cart in self.carts:
            if cart not in removed:
                self.cart_track(cart).cart = None

                cart.move()

                track = self.cart_track(cart)

                if self.has_collision(cart):
                    collision = (cart.j, cart.i)

                    removed.append(track.cart)
                    removed.append(cart)
                    
                    track.cart = None
                else:
                    if track.get_char() not in '-|':
                        self.turn_cart(cart)

                    track.cart = cart

        for cart in removed:
            self.carts.remove(cart)

        return collision

    def first_collision(self):
        collision = None
        while collision is None:
            collision = self.tick()
        return collision

    def last_standing(self):
        while len(self.carts)>1:
            self.tick()

        last = self.carts[0] 
        return (last.j, last.i)

def main():
    with open('input.txt', 'r') as raw_data:
        track_map = Track_Map(raw_data)

        print(track_map.first_collision())
        print(track_map.last_standing())
        

if __name__ == '__main__':
    main()