from mininet.topo import Topo
from mininet.net import Mininet
from mininet.cli import CLI
from mininet.node import RemoteController

class MyFirstNetwork(Topo):
    def build(self):
        switch1 = self.addSwitch('s1')

        for host in ['h1', 'h2', 'h3']:
            self.addHost(host)
            self.addLink(switch1, host)

if __name__ == '__main__':
    net = Mininet(MyFirstNetwork(), controller = RemoteController)
    net.start()
    # control and manage a virtual network from a single console
    CLI(net)
    net.stop()

#important command:
#dump: to see IP adress of all things
#h1 ping h2: h1 talk to p2
#pingall: nodes ping all together
#sudo python3 new_network.py

#xterm h1 h2