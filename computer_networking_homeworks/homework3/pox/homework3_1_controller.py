from pox.core import core
import pox.lib.packet as pkt
import pox.openflow.libopenflow_01 as of

class Homework31Controller:
    def __init__(self) -> None:
        core.openflow.addListeners(self)

    def _handle_ConnectionUp(self, event):
        connection = event.connection

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
                match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = pkt.ipv4.ICMP_PROTOCOL, nw_dst = '10.0.0.1'),
                action = of.ofp_action_output(port = 1),
            )
        )

        connection.send(
            of.ofp_flow_mod(
                match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = pkt.ipv4.ICMP_PROTOCOL, nw_dst = '10.0.0.2'),
                action = of.ofp_action_output(port = 2),
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
    core.registerNew(Homework31Controller)

#python3 pox.py controller