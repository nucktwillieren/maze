import sys
import matplotlib.pyplot as plt
from matplotlib.font_manager import FontProperties

class Mouse:
    @staticmethod
    def go(maze, start, end):
        route = []
        Mouse.visit(maze, start, end, route)
        return route
    
    @staticmethod
    def visit(maze, pt, end, route):
        if Mouse.isVisitable(maze, pt, route):
            route.append(pt)
            print(route)
            if not Mouse.isEnd(route, end) and \
               not Mouse.tryOneOut(maze, pt, end, route):
                print(pt)
                route.pop()
                print(pt,'delete')
        return Mouse.isEnd(route, end)
    
    @staticmethod
    def isVisitable(maze, pt, route):
        return maze[pt[0]][pt[1]] == 0 and pt not in route
        
    @staticmethod
    def isEnd(route, end):
        return end in route
        
    @staticmethod
    def tryOneOut(maze, pt, end, route):
        return Mouse.visit(maze, (pt[0], pt[1] + 1), end, route) or \
               Mouse.visit(maze, (pt[0] + 1, pt[1]), end, route) or \
               Mouse.visit(maze, (pt[0], pt[1] - 1), end, route) or \
               Mouse.visit(maze, (pt[0] - 1, pt[1]), end, route)
class Maze:
    
    maze = list()
    minus_block = list()
    filename = ""

    start_x = 0
    start_y = 0
    end_x = 0
    end_y = 0
    maze_length = 0
    maze_line_count = 0

    def loadMaze(self):               
        with open(self.filename) as my_maze:
            line = my_maze.readline()
            x = 0
            for words in line:
                if words == '-':
                    self.minus_block.append(x)
                x = x + 1           
        my_maze.close()

    def readMaze(self):
        length = 0   
        with open(self.filename) as my_maze:
            i = 0
            for line in my_maze:
                self.maze.append(list())
                length = len(line)
                j = 0
                while j < length:
                    words = line[j]
                    double_words = line[j:j+2]
                    if j not in self.minus_block:
                        if words == " ":
                            self.maze[i].append(0)
                    if double_words == "  " :
                        self.maze[i].append(0)
                        j = j + 1
                    elif words == "+" or words =="|":
                        self.maze[i].append(2)
                    elif double_words == "--":
                        self.maze[i].append(2)
                        j = j + 1
                    j = j + 1
                i = i + 1  
            self.maze_line_count = i                
            my_maze.close()
            
    def __init__(self,filename):
        self.maze = list()
        self.minus_block = list()
        self.filename = filename
        self.loadMaze()        
        self.readMaze()    
        self.maze_length = len(self.maze[0])

def main():
    sys.setrecursionlimit(3000)
    files = ['maze_20by20','maze_100by100']
    size = [20,100]
    
    index = 0
    for filename in files:
        maze = Maze(filename)
        end_x = maze.maze_length-1
        end_y = maze.maze_line_count-2
        maze = maze.maze        
        
        print(Mouse.go(maze, (1,0), (end_y,end_x)))

        for pt in Mouse.go(maze, (1,0), (end_y,end_x)):
            maze[pt[0]][pt[1]] = 1        
        
        fig = plt.figure(frameon=False)
        ax = fig.add_axes([0, 0, 0.97, 0.97])
        ax.axis('off')
            
        y = 0
        for row in maze:
            y = y + 1
            x = 0
            for block in row:
                x = x + 1
                {
                    0 : lambda: plt.text(float(x)/(end_x+1),1-float(y)/(end_y+2)," ",fontsize=50*100/size[index]),
                    1 : lambda: plt.text(float(x)/(end_x+1),1-float(y)/(end_y+2),"■",fontsize=50*100/size[index],color="blue"),
                    2 : lambda: plt.text(float(x)/(end_x+1),1-float(y)/(end_y+2),"■",fontsize=50*100/size[index]),
                }[block]()                
        
        fig = plt.gcf()
        my_dpi = fig.get_dpi()
        fig.set_size_inches(7840.0/float(my_dpi),7840.0/float(my_dpi))
        plt.savefig(filename+'.png')
        plt.close()
        
        index = index + 1
        

if __name__ == '__main__':
    main()