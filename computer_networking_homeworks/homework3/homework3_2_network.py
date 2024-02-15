from mininet.topo import Topo
from mininet.net import Mininet
from mininet.cli import CLI
from mininet.node import RemoteController

class Homework32Network(Topo):
    def build(self):
        switch1 = self.addSwitch('s1')
        switch2 = self.addSwitch('s2')
        switch3 = self.addSwitch('s3')
        host1 = self.addHost('h1', ip = '10.0.0.1')
        host2 = self.addHost('h2', ip = '10.0.0.2')
        host3 = self.addHost('h3', ip = '10.0.0.3')

        self.addLink(switch1, host1, port1 = 4, port2 = 1)
        self.addLink(switch2, host2, port1 = 4, port2 = 1)
        self.addLink(switch3, host3, port1 = 4, port2 = 1)
        self.addLink(switch1, switch2, port1 = 1, port2 = 1)
        self.addLink(switch1, switch3, port1 = 2, port2 = 2)
        self.addLink(switch2, switch3, port1 = 3, port2 = 3)

if __name__ == '__main__':
    net = Mininet(Homework32Network(), controller = RemoteController)
    net.start()
    CLI(net)
    net.stop()