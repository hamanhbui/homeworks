from mininet.topo import Topo
from mininet.net import Mininet
from mininet.cli import CLI
from mininet.node import RemoteController

class Homework31Network(Topo):
    def build(self):
        switch1 = self.addSwitch('s1')
        host1 = self.addHost('h1', ip = '10.0.0.1')
        host2 = self.addHost('h2', ip = '10.0.0.2')

        self.addLink(switch1, host1)
        self.addLink(switch1, host2)

if __name__ == '__main__':
    net = Mininet(Homework31Network(), controller = RemoteController)
    net.start()
    CLI(net)
    net.stop()