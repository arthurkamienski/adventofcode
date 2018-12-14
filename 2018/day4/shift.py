import re

# converts the file strings to a list of events
def to_list(data):
	events = []

	for entry in data:
		# finds relevant info and stores into the list
		match = re.search('\[(.+)\-(.+)\-(.+)\ (.+):(.+)\]\ (.+)$', entry)
		event = list(match.groups())

		events.append(event)

	# sorts to get the events into cronological order
	events.sort()
	# removes the dates after sorting (not necessary anymore)
	events = [e[3:6] for e in events]

	return events

# calculates the intervals in which each guard sleeps
# returns a dict containing a list of intervals for every guard
# interval = (end_time, start_time)
def get_intervals(events):
	# dict of lists of intervals slept by each guard
	intervals = {}

	number = None
	start = None

	for event in events:
		# looks for the beginning of a shift
		match = re.search('#(.+)\ begins', event[2])
		
		# if it finds a shift beginning
		if match:
			# initializes the list of sleep intervals for the guard
			# if it has not done so yet
			number = int(match.group(1))
			intervals[number] = intervals.get(number, [])
		elif event[2] == 'falls asleep':
			# stores the beginning of a sleeping interval
			start = int(event[1])
		elif event[2] == 'wakes up':
			# stores the end time
			end = int(event[1])
			# creates a new interval
			interval = (end, start)
			# adds the interval for the given guard
			intervals[number].append(interval)

	return intervals

# calculates the guard that slept the most
def max_sleeper(intervals):
	max_time = 0
	sleeper = None

	for guard in intervals:
		# calculates the total sleep time
		sleep_time = sum([a-b for a,b in intervals[guard]])

		# updates the max sleep time and the
		# max sleeper if it has slept more than
		# the current one
		if sleep_time > max_time:
			max_time = sleep_time
			sleeper = guard

	return sleeper

# calculates which is the minute most slept by a guard
# using bucket count
def common_minute(guard, intervals):
	time = [0] * 60

	for a,b in intervals[guard]:
		for i in range(b, a):
			time[i] = time[i] + 1

	# returns the maximum value
	return time.index(max(time))

# calculates the guard and the minute with most occurances
def most_times(intervals):
	time = {}

	# for each interval, mark store the guard
	# that was asleep and the number of times
	for guard in intervals:
		for a,b in intervals[guard]:
			for i in range(b, a):
				# dict for the number of times for each guard in that
				# minute
				time[i] = time.get(i, {})
				time[i][guard] = time[i].get(guard, 0)
				time[i][guard] = time[i][guard] + 1

	# gets the guard that slept the most times
	# and the max amount of times for each minute
	for minute in time:
		# gets the max based on the second column of the tuple
		time[minute] = max(time[minute].items(), key=lambda x: x[1])

	# gets the minute which was slept the most times
	# and the tuple containing the guard and amount of times
	minute, tup = max(time.items(), key=lambda x: x[1][1])
	guard = tup[0]

	return (guard, minute)

def main():
	with open('input.txt', 'r') as raw_data:
		events = to_list(raw_data)

		sleep_intervals = get_intervals(events)

		sleeper = max_sleeper(sleep_intervals)
		common = common_minute(sleeper, sleep_intervals)

		print('Guard * minute most slept: ' + str(common*sleeper))

		guard, minute = most_times(sleep_intervals)

		print('Guard * minute most times: ' + str(guard*minute))

if __name__ == '__main__':
	main()