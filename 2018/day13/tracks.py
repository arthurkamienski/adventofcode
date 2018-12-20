class Track:
    """Models a piece track of the track map"""
    def __init__(self, char, cart=None):
        # char that represents the model
        self.char = char
        # cart object stationed at this track (None if none)
        self.cart = cart

    # returns its own char if there are no carts
    # returns the cart char otherwise
    def get_char(self):
        if self.cart is None:
            char = self.char
        else:
            char = self.cart.get_char()

        return char

class Cart:
    """Models the carts that rides the tracks"""
    def __init__(self, i, j, char):
        # current position
        self.i = i
        self.j = j
        # the number of the turn
        self.turn_num = 0

        # current speed
        self.speed_x = 0
        self.speed_y = 0

        # variable for easier identification of the position
        self.vertical = False

        # speed:
        # up:   ( 0, -1)
        # left: (-1,  0)
        # down: ( 0,  1)
        # right:( 1,  0)
        if char == '^':
            self.speed_y = -1
            self.vertical = True
        elif char == 'v':
            self.speed_y = 1
            self.vertical = True
        elif char == '<':
            self.speed_x = -1
        elif char == '>':
            self.speed_x = 1

    # Decides what action to take on a crossing based on turn number
    def crossing(self):
        # if it is the first or the third crossing, turn left/right (respect.)
        # if it is the second, do nothing (continue straight)
        if self.turn_num == 0:
            self.turn('left')
        elif self.turn_num == 2:
            self.turn('right')
            self.turn_num = -1

        self.turn_num = self.turn_num+1

    # turn the cart in given direction
    def turn(self, direction):
        x = self.speed_x

        # if it turns it changes direction
        self.vertical = not self.vertical
        
        # turn conversion:
        # y = x
        # x = +/-y (left/right)
        if direction == 'right':
            self.speed_x = -self.speed_y
            self.speed_y = x
        else:
            self.speed_x = self.speed_y
            self.speed_y = -x

    # returns the char representing the cart based on the direction it is moving
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

    # move the cart in the direction it is moving
    def move(self):
        # adds speed i and j to its position
        self.i = self.i + self.speed_y
        self.j = self.j + self.speed_x

class Track_Map:
    def __init__(self, raw_data):
        # list of carts
        self.carts = []

        self.build_layout(raw_data)

    # builds a track layout based on the data of the input file
    def build_layout(self, raw_data):
        # dict -> O(1) indexing with blank spaces (not all of the coords have tracks)
        self.layout = {}

        for i, row in enumerate(raw_data):
            # initializes row
            self.layout[i] = {}
            for j, char in enumerate(row):
                # if not ' ' or '\n'
                if char not in ' \n':
                    # if it is a cart
                    if char in '><v^':
                        # creates a new cart
                        cart = Cart(i, j, char)
                        self.carts.append(Cart(i, j, char))

                        # palces a track in place of the cart according to the cart direction
                        if cart.vertical:
                            self.layout[i][j] = Track('|', cart)
                        else:
                            self.layout[i][j] = Track('-', cart)
                    else:
                        # if not a cart just places a track
                        self.layout[i][j] = Track(char)

    # print the track map
    def print_map(self):
        for i, line in self.layout.items():
            k = 0
            for j, char in line.items():
                # adjust tracks spacing with blank spaces
                while k < j:
                    print(' ', end='')
                    k = k + 1
                print(self.layout[i][j].get_char(), end='')
                k = k + 1
            print()

    # prints the position of all the carts in the cart list
    def print_cart_positions(self):
        print([(c.i, c.j) for c in self.carts])

    # checks if there is a collision
    def has_collision(self, cart):
        # there is a collision if there is already a cart in the track of the given cart
        return self.cart_track(cart).cart is not None

    # returns the track on the position of the cart
    def cart_track(self, cart):
        return self.layout[cart.i][cart.j]

    # decides whether to turn cart vased on the track it is standing
    def turn_cart(self, cart):
        # get the char representing the track
        track = self.cart_track(cart).get_char()

        # if it a curve
        if track == '\\':
            # if it is going vertical, turn left
            # otherwise turn right
            if cart.vertical:
                cart.turn('left')
            else:
                cart.turn('right')
        elif track == '/':
            # the opposite of the curve above
            if cart.vertical:
                cart.turn('right')
            else:
                cart.turn('left')
        elif track == '+':
            cart.crossing()

    def tick(self):
        # collision variable to be returned if there is a collision
        collision = None

        # carts to be removed after a collision
        removed = []
        
        # sorts the carts so that they are moved from left to right, top to bottom
        self.carts.sort(key=lambda x: (x.j, x.i))
        
        for cart in self.carts:
            # do not move removed carts (the carts are removed only after the iteration
            # so as not to mess with the iteration process)
            if cart not in removed:
                # remove cart from previous track
                self.cart_track(cart).cart = None

                cart.move()

                # new track
                track = self.cart_track(cart)

                if self.has_collision(cart):
                    # only the first collision needed
                    # (does not update if there is already one)
                    if collision is None:
                        collision = (cart.j, cart.i)

                    # removes both carts
                    removed.append(track.cart)
                    removed.append(cart)
                    
                    # removes cart from track
                    track.cart = None
                else:
                    if track.get_char() not in '-|':
                        self.turn_cart(cart)

                    # if everything is okay, puts cart on next tracks
                    track.cart = cart

        # remove each cart that collided
        for cart in removed:
            self.carts.remove(cart)

        return collision

    # ticks until first collision and return the position in which it occured
    def first_collision(self):
        collision = None
        while collision is None:
            collision = self.tick()
        return collision

    # runs until there is only one cart left and returns its position
    def last_standing(self):
        while len(self.carts)>1:
            self.tick()

        last = self.carts[0] 
        return (last.j, last.i)

def main():
    with open('input.txt', 'r') as raw_data:
        track_map = Track_Map(raw_data)

        print('First collision: ' + str(track_map.first_collision()))
        print('Last cart: ' + str(track_map.last_standing()))
        

if __name__ == '__main__':
    main()