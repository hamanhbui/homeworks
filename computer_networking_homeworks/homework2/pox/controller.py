from pox.core import core
import pox.lib.packet as pkt
import pox.openflow.libopenflow_01 as of

class MyController:
    def __init__(self) -> None:
        core.openflow.addListeners(self)

    def _handle_ConnectionUp(self, event):
        connection = event.connection
        # Exercise 2
        # connection.send(
        #     of.ofp_flow_mod(
        #         match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = pkt.ipv4.ICMP_PROTOCOL),
        #         action = of.ofp_action_output(port = 65530),
        #     )
        # )

        # connection.send(
        #     of.ofp_flow_mod(
        #         match = of.ofp_match(dl_type=pkt.ethernet.ARP_TYPE),
        #         action = of.ofp_action_output(port = 65531),
        #     )
        # )

        # Exercise 3
        #Forward port 1 to port 2
        connection.send(
            of.ofp_flow_mod(
                match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = 6, nw_dst = '10.0.0.1'),
                action = of.ofp_action_output(port = 1),
            )
        )

        connection.send(
            of.ofp_flow_mod(
                match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = 6, nw_dst = '10.0.0.2'),
                action = of.ofp_action_output(port = 2),
            )
        )

        connection.send(
            of.ofp_flow_mod(
                match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = 6, nw_dst = '10.0.0.3'),
                action = of.ofp_action_output(port = 3),
            )
        )

        # discovre the link layer address
        connection.send(
            of.ofp_flow_mod(
                match = of.ofp_match(dl_type=pkt.ethernet.ARP_TYPE),
                action = of.ofp_action_output(port = 65531),
            )
        )

def launch():
    core.registerNew(MyController)

#python3 pox.py controller