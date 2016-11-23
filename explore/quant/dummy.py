import sys
import jqremote as jqr

jqr.setup()

def initialize(context):
    jqr.init(context, "dummy")

def before_trading_start(context):
    jqr.set_slip_fee(context)

def handle_data(context, data):
    jqr.handle_data(context, data)
