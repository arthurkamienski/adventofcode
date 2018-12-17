class Node:
    """One node of the tree"""
    def __init__(self):
        self.children = []
        self.metadata = []

    def add_child(self, node):
        self.children.append(node)

    def add_metadata(self, meta):
        self.metadata.append(meta)

    # sums the metadata for this node's list
    # and recursively to each of the node's children
    def meta_sum(self):
        # sum of this node's metadata
        total = sum(self.metadata)

        # recursive sum of childrens' metadata
        for child in self.children:
            total = total + child.meta_sum()

        return total

    # calculates the value of the node
    # value:
    # if no children = sum(metadata)
    # else = sum of childrens' values indexed by the metadata
    def value(self):
        value = 0
        if self.children == []:
            value = sum(self.metadata)
        else:
            for index in self.metadata:
                if index > 0 and index <= len(self.children):
                    value = value + self.children[index-1].value()

        return value

class Tree:
    """Contains reference to root node and tree building method"""
    def __init__(self, numbers):
        self.root = None
        self.build_tree(numbers)

    # new_done is recursive -> new tree starts from the root
    def build_tree(self, numbers):
        self.root = self.new_node(numbers)

    # creates a new node based on the list on numbers (recursive)
    def new_node(self, numbers):
        # first number is the number of children
        children_num = numbers.pop(0)
        # second number is the number of metadata
        meta_num = numbers.pop(0)
        # creates new node
        node = Node()

        # call creates a new node for each child (recursive)
        for i in range(children_num):
            node.add_child(self.new_node(numbers))

        # reads the metadata and adds it to the node
        for i in range(meta_num):
            node.add_metadata(numbers.pop(0))

        return node

    # get sum of the metadata for the tree
    def sum_metadata(self):
        return self.root.meta_sum()

# converts input to a list of integers
def to_list(raw_data):
    # reads one line from the file, clears '\n', splits on ' ',
    # converts everything to int
    return list(map(lambda x: int(x), raw_data.readline().strip().split(' ')))

def main():
    with open('input.txt', 'r') as raw_data:
        numbers = to_list(raw_data)
        tree = Tree(numbers)
        print("Sum of all the metadata: " + str(tree.sum_metadata()))
        print("Root node value: " + str(tree.root.value()))

if __name__ == '__main__':
    main()