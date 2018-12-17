import re
class Step:
    """Class modeling a instruction of the instruction set"""
    def __init__(self, label):
        self.label = label
        # list of steps which this step blocks
        self.blocks = []
        # list of steps which block this step
        self.blocked_by = []

    # checks if this step is block
    def is_blocked(self):
        return self.blocked_by != []

    # blocks a step
    def block(self, step):
        self.blocks.append(step)

    # blocks this step by the other
    def block_by(self, step):
        self.blocked_by.append(step)

    # removes the given step that blocks it
    def remove(self, step):
        if step in self.blocked_by:
            self.blocked_by.remove(step)

    # removes this step's block from each of the steps it is blocking
    def unblock(self):
        for step in self.blocks:
            step.remove(self)

class Manager:
    """Models the list of instructions"""
    def __init__(self, instr_set):
        # dict label => object
        self.steps = {}
        # undone steps
        self.undone = []
        # sequence of steps
        self.sequence = ''

        self.load_dependencies(instr_set)

    # loads the dependency list and initializes the undone list
    def load_dependencies(self, instr_set):
        for step_a, step_b in instr_set:
            self.add_dependancy(step_a, step_b)
        self.set_undone()

    # adds a step to the sequence of steps
    def add_to_sequence(self, step):
        self.sequence = self.sequence + step.label

    # resets the list of undone steps
    def set_undone(self):
        self.undone = list(self.steps.items())

    # given a label, returns a object
    # if it exists, returns the object. Else creates a new one
    def get_step(self, label):
        if label not in self.steps.keys():
            self.steps[label] = Step(label)

        return self.steps[label]

    # adds a dependency between two steps (a blocks b)
    def add_dependancy(self, a, b):
        instr_a = self.get_step(a)
        instr_b = self.get_step(b)

        instr_a.block(instr_b)
        instr_b.block_by(instr_a)

    # returns an ordered list of the step names with no dependencies/blocks
    def no_dependancies(self):
        # step[1] = obj, step[0] = label
        steps = [step for step in self.undone if not step[1].is_blocked()]
        steps.sort()
        return steps

    # returns the next step to be executed (first in no dependencies list)
    def next_step(self):
        step = None
        step_list = self.no_dependancies()

        if step_list != []:
            step = step_list[0]

        return step

    # executes a step, removing it from the instructions,
    # adding to the step sequence
    # and unblocking the steps it blocks
    def execute(self, step):
        self.remove_step(step)
        self.complete(step)

    # removes the step from the list of steps
    def remove_step(self, step):
        self.undone.remove((step.label, step))

    # completes a step, unblocking it and adding to the sequence
    def complete(self, step):
        step.unblock()
        self.add_to_sequence(step)

    # finds the sequence of steps to be executed
    def find_sequence(self):
        while self.undone != []:
            name, obj = self.next_step()
            self.execute(obj)

    # assigns a worker to work on the next free step
    def assign_step(self, worker):
        step = self.next_step()

        # if there is a free step
        if step != None:
            # get the object from the tuple (label, obj)
            step = step[1]
            self.remove_step(step)

        worker.execute_step(step)

    # assigns available steps to idle workers
    def assign_steps(self, idle_workers):
        while idle_workers != [] and self.no_dependancies() != []:
            worker = idle_workers.pop()
            self.assign_step(worker)

    # makes workers on the list of workers execute the steps
    # until it is done (all workers idle, all steps done)
    # returns the time it took
    def build(self, workers):
        idle_workers = [w for w in workers if w.is_idle()]

        time = 0

        self.assign_steps(idle_workers)

        # while there are workers working and there are steps to be done
        while len(idle_workers) != len(workers) or self.undone != []:
    
            # makes 1 second pass
            for worker in workers:
                worker.tick(self)

            # checks for new idle workers
            idle_workers = [w for w in workers if w.is_idle()]

            # reassigns steps
            self.assign_steps(idle_workers)

            # for debugging
            # print(time, end=' ')
            # for worker in workers:
            #   if worker.step != None:
            #       print(worker.step.label, end=' ')
            #   else:
            #       print('.', end=' ')
            # print(self.sequence)

            time = time + 1

        return time
            

class Worker:
    def __init__(self):
        # step in which it is working on
        self.step = None
        self.time_to_complete = 0

    # sets the step to be executed and the time it takes to do so
    def execute_step(self, step):
        self.step = step
        # print('Working on step: ' + step.label)
        self.time_to_complete = 60 + ord(step.label) - 63

    # checks if it is not working on any step
    def is_idle(self):
        return self.step == None

    # subtracts one from the time it takes to complete the step
    # if it is 1, complete the task
    def tick(self, instructions):
        if self.step != None:
            self.time_to_complete = self.time_to_complete - 1

            if self.time_to_complete == 1:
                # print(self.step.label + ' done')
                instructions.complete(self.step)
                self.step = None

# given the raw input, extract the steps and the dependencies to a list
def extract(raw_data):
    instrs = []

    for line in raw_data:
        # selects two isolated uppercase chars in a line
        steps = re.search('.*\ ([A-Z])\ .*\ ([A-Z])\ .*$', line).groups()
        instrs.append(steps)

    return instrs
        
def main():
    with open('input.txt', 'r') as raw_data:
        instr_set = extract(raw_data)

        manager = Manager(instr_set)
        manager.find_sequence()

        print('Execute order: ' + manager.sequence)

        manager = Manager(instr_set)
        workers = [Worker() for i in range(5)]
        print('Time taken by 5 workers: ' + str(manager.build(workers)))

if __name__ == '__main__':
    main()
        