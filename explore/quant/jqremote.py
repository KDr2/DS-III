# -*- encoding: utf-8 -*-

import inspect
import datetime

import requests

# API Setup

_jqapi_names = [
    'g', 'log',
    'set_option', 'set_slippage', 'set_commission',
    'order_target_value',
    'send_message',
    'FixedSlippage', 'PerTrade',
]

_mod_runner = None

def import_jqapi(scope):
    if not _mod_runner:
        raise Exception("Please call setup in your entry file first!")
    if scope.get('g'): # imported
        return
    for name in _jqapi_names:
        scope[name] = getattr(_mod_runner, name)

def setup():
    global _mod_runner
    frm = inspect.stack()[1]
    _mod_runner = inspect.getmodule(frm[0])
    import_jqapi(globals())


## get data from remote

def get_stocks(strategy):
    response = requests.get("http://depot.kdr2.com/tafm/{}.sym".format(strategy))
    symbols = response.text.split("\n")
    symbols = [sym.strip() for sym in symbols]
    return [sym for sym in symbols if sym]

def get_signal_csv(strategy, date_start=None, date_end=None):
    response = requests.get("http://depot.kdr2.com/tafm/{}.sig".format(strategy))
    lines = response.text.split("\n")
    for line in lines:
        row = line.split(",")
        if len(row) != 3:
            continue
        date_sigs = g.signals.setdefault(row[0].strip(), {})
        date_sigs[row[1].strip()] = int(row[2].strip())

    return g.signals

def get_signal(context, strategy, date):
    date = date.strftime("%Y%m%d")
    if (date not in g.signals):
        get_signal_csv(strategy)
    return g.signals.get(date, {})


## hooks for joinquant

# init
def init(context, strategy):
    g.strategy = strategy
    g.security = get_stocks(strategy)
    g.signals = {}
    set_backtest()


# backtest options
def set_backtest():
    set_option('use_real_price', True) #用真实价格交易
    log.set_level('order', 'error')



# setup fee, should be called in before_trading_start
def set_slip_fee(context):
    set_slippage(FixedSlippage(0))

    dt = context.current_dt
    if dt > datetime.datetime(2013, 1, 1):
        set_commission(PerTrade(buy_cost=0.0003, sell_cost=0.0013, min_cost=5))
    elif dt > datetime.datetime(2011, 1, 1):
        set_commission(PerTrade(buy_cost=0.001, sell_cost=0.002, min_cost=5))
    elif dt > datetime.datetime(2009, 1, 1):
        set_commission(PerTrade(buy_cost=0.002, sell_cost=0.003, min_cost=5))
    else:
        set_commission(PerTrade(buy_cost=0.003, sell_cost=0.004, min_cost=5))


def handle_data(context, data):
    N = len(g.security)
    capital_unit = context.portfolio.portfolio_value / N
    date_target = context.previous_date
    signals = get_signal(context, g.strategy, date_target)

    for sym, sig in signals.items():
        if sig == -1:
            order_target_value(sym, 0)
        elif sig == 1:
            order_target_value(sym, capital_unit)
        else:
            send_message("No operation for [{}]".format(sym))
