/**
 * @module PassiveClientComm
 *
 * use in python:
 * from ipykernel.comm.comm import Comm
 * c=Comm(target_name='PassiveClientComm', target_module='scripts/comm-passive-client')
 * c.send({'x':'y'})
 */

define(function (require) {
    "use strict";

    var PassiveClientComm = function (comm, msg) {
        console.log("==COMM OPEN== : ", comm, msg)
        comm.on_msg(function(m){console.log(m);})
    };

    return {'PassiveClientComm': PassiveClientComm};
});
