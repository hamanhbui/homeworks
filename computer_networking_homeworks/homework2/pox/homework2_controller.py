from pox.core import core
import pox.lib.packet as pkt
import pox.openflow.libopenflow_01 as of

class Homework2Controller:
    def __init__(self) -> None:
        core.openflow.addListeners(self)

    def _handle_ConnectionUp(self, event):
        connection = event.connection
        if event.dpid == 1:
            connection.send(
                of.ofp_flow_mod(
                    match = of.ofp_match(dl_type=phomework2_controllerkt.ethernet.IP_TYPE, nw_proto = pkt.ipv4.ICMP_PROTOCOL, nw_dst = '10.0.0.1'),
                    action = of.ofp_action_output(port = 2),
                )
            )

            connection.send(
                of.ofp_flow_mod(
                    match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = pkt.ipv4.ICMP_PROTOCOL, nw_dst = '10.0.0.2'),
                    action = of.ofp_action_output(port = 3),
                )
            )

            connection.send(
                of.ofp_flow_mod(
                    match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = pkt.ipv4.ICMP_PROTOCOL, nw_dst = '10.0.0.3'),
                    action = of.ofp_action_output(port = 1),
                )
            )

            connection.send(
                of.ofp_flow_mod(
                    match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = pkt.ipv4.ICMP_PROTOCOL, nw_dst = '10.0.0.4'),
                    action = of.ofp_action_output(port = 1),
                )
            )
            
        else:
            connection.send(
                of.ofp_flow_mod(
                    match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = pkt.ipv4.ICMP_PROTOCOL, nw_dst = '10.0.0.1'),
                    action = of.ofp_action_output(port = 1),
                )
            )

            connection.send(
                of.ofp_flow_mod(
                    match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = pkt.ipv4.ICMP_PROTOCOL, nw_dst = '10.0.0.2'),
                    action = of.ofp_action_output(port = 1),
                )
            )

            connection.send(
                of.ofp_flow_mod(
                    match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = pkt.ipv4.ICMP_PROTOCOL, nw_dst = '10.0.0.3'),
                    action = of.ofp_action_output(port = 2),
                )
            )

            connection.send(
                of.ofp_flow_mod(
                    match = of.ofp_match(dl_type=pkt.ethernet.IP_TYPE, nw_proto = pkt.ipv4.ICMP_PROTOCOL, nw_dst = '10.0.0.4'),
                    action = of.ofp_action_output(port = 3),
                )
            )

        connection.send(
            of.ofp_flow_mod(
                match = of.ofp_match(dl_type=pkt.ethernet.ARP_TYPE),
                action = of.ofp_action_output(port = 65531),
            )
        )

def launch():
    core.registerNew(Homework2Controller)