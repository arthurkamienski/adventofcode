class Marble:
	"""Linked list element"""
	def __init__(self, number, left=None, right=None):
		# left and right marbles (self for ball 0)
		if right is None:
			self.right = self
		else:
			self.right = right

		if left is None:
			self.left = self
		else:
			self.left = left

		self.number = number

	def get_number(self):
		return self.number

	def get_left(self):
		return self.left

	def get_right(self):
		return self.right

	def set_left(self, left):
		self.left = left

	def set_right(self, right):
		self.right = right

class Game:
	def __init__(self, players, marbles):
		# player scores
		self.scoreboard = {}

		# number of player and marbles
		self.players = players
		self.marbles = marbles

		# next player/marble number to play
		self.next_player = 1
		self.next_marble = 0

		# first marble (print starts from this)
		self.start_marble = Marble(0)

		# current marble
		self.curr_marble = self.start_marble

	# inserts marble in position
	def insert_marble(self):
		# left and right marbles to the inserted marble
		left_marble = self.curr_marble.get_right()
		right_marble = left_marble.get_right()

		new_marble = Marble(self.next_marble, left_marble, right_marble)
		
		# list insert
		left_marble.set_right(new_marble)
		right_marble.set_left(new_marble)

		# the new marble is the current
		self.curr_marble = new_marble

	# removes a marble 7 positons anti clockwise and returns its value
	def remove_marble(self):
		# get the 7th marble to the right
		removed_marble = self.curr_marble
		for i in range(7):
			removed_marble = removed_marble.get_left()

		# left and right to the removed marble
		left = removed_marble.get_left()
		right = removed_marble.get_right()

		# remove from list
		left.set_right(right)
		right.set_left(left)

		# currurent marble is the one to the right of the removed
		self.curr_marble = right

		# return the number of the removed
		return removed_marble.get_number()

	# adds the value to the player score
	def add_score(self, player, value):
		# if player is not in dict, score = 0
		self.scoreboard[player] = self.scoreboard.get(player, 0) + value

	# plays one turn of the game
	def turn(self):
		if self.next_marble % 23 == 0:
			# adds the value of the current marble and the value from the removed marble
			self.add_score(self.next_player, self.next_marble + self.remove_marble())
		else:
			self.insert_marble()

		# updates next marble to be put
		self.next_marble = self.next_marble + 1
		# updates next player to play (loops from 1 to self.players)
		self.next_player = self.next_player % self.players + 1

	# calls self.turn until the last ball is reached
	def play(self):
		while self.next_marble != self.marbles+1:
			# feedback
			print("Playing marble " + str(self.next_marble), end = '\r')
			self.turn()
		print()

	# prints circle (list of marble numbers) starting from the start marble
	def print_circle(self):
		# print start marble
		print(self.start_marble.get_number(), end='')
		# start from second marble
		curr = self.start_marble.get_right()

		# while not back to first marble
		while curr != self.start_marble:
			# print and move to the next
			print(', ' + str(curr.get_number()), end='')
			curr = curr.get_right()
		print()

	# return the max score reached
	def max_score(self):
		return max(self.scoreboard.values())


def main():
	# input: 418 players; last marble is worth 71339 points
	players = 418
	marbles = 71339

	game = Game(players, marbles)
	print('Game: ' + str(players) + ' players, ' + str(marbles) + ' marbles.')
	game.play()
	print('Max score:' + str(game.max_score()))

	marbles = marbles*100

	game = Game(players, marbles)
	print('Game: ' + str(players) + ' players, ' + str(marbles) + ' marbles.')
	game.play()
	print('Max score:' + str(game.max_score()))


if __name__ == '__main__':
	main()