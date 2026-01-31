#!/usr/bin/env python3
"""
Strategy Decision Engine Rules Module
=====================================
This module contains all business rules for the strategy decision engine.
It is called by the COBOL program STRATPY to evaluate account definitions,
risk segmentation, and strategic activities.

Usage:
    python strategy_rules.py <account_number> <status> <balance> <phone> <consent> <score>

Output:
    workable|dialable|risk_segment|small_balance|activity|dial_frequency
"""

import sys


def determine_workable(status: str) -> str:
    """
    Determine if account is workable based on status.
    An account is workable if status is OPEN.
    
    Args:
        status: Account status (OPEN, CLOSE, BANKRUPT, DEFAULT, FRAUD)
    
    Returns:
        'Y' if workable, 'N' otherwise
    """
    return 'Y' if status.strip().upper() == 'OPEN' else 'N'


def determine_dialable(phone_number: str, phone_consent: str) -> str:
    """
    Determine if account is dialable.
    An account is dialable if it has a valid phone number AND consent to dial.
    
    Args:
        phone_number: Account phone number
        phone_consent: Phone consent flag (Y/N)
    
    Returns:
        'Y' if dialable, 'N' otherwise
    """
    has_valid_phone = phone_number.strip() != '' and phone_number.strip() != '0' * len(phone_number.strip())
    has_consent = phone_consent.strip().upper() == 'Y'
    return 'Y' if has_valid_phone and has_consent else 'N'


def determine_risk_segment(score: int) -> str:
    """
    Determine risk segment based on score.
    
    Args:
        score: 3-digit risk score
    
    Returns:
        Risk segment: 'HIGH', 'MEDIUM', 'LOW', or 'NONE'
    """
    if score > 300:
        return 'HIGH'
    elif score > 200:
        return 'MEDIUM'
    elif score > 100:
        return 'LOW'
    else:
        return 'NONE'


def determine_small_balance(balance: float) -> str:
    """
    Determine if account has a small balance.
    Balance < 250 is considered small.
    
    Args:
        balance: Account balance
    
    Returns:
        'Y' if small balance, 'N' otherwise
    """
    return 'Y' if balance < 250.0 else 'N'


def determine_strategy(workable: str, dialable: str, risk_segment: str, small_balance: str) -> tuple:
    """
    Determine strategic activity and dial frequency.
    
    Rules:
    - If workable AND dialable AND NOT small balance:
      - Activity = 'DIALABLE'
      - Dial frequency based on risk:
        - HIGH = 4 times/day
        - MEDIUM = 3 times/day
        - LOW = 2 times/day
    - Otherwise: Activity = 'NO DIAL', frequency = 0
    
    Args:
        workable: Workable flag (Y/N)
        dialable: Dialable flag (Y/N)
        risk_segment: Risk segment (HIGH/MEDIUM/LOW/NONE)
        small_balance: Small balance flag (Y/N)
    
    Returns:
        Tuple of (activity, dial_frequency)
    """
    if workable == 'Y' and dialable == 'Y' and small_balance != 'Y':
        activity = 'DIALABLE'
        if risk_segment == 'HIGH':
            dial_frequency = 4
        elif risk_segment == 'MEDIUM':
            dial_frequency = 3
        elif risk_segment == 'LOW':
            dial_frequency = 2
        else:
            dial_frequency = 0
    else:
        activity = 'NO DIAL'
        dial_frequency = 0
    
    return activity, dial_frequency


def evaluate_account(account_number: str, status: str, balance: float, 
                     phone_number: str, phone_consent: str, score: int) -> dict:
    """
    Evaluate all rules for an account and return results.
    
    Args:
        account_number: Account identifier
        status: Account status
        balance: Account balance
        phone_number: Phone number
        phone_consent: Phone consent flag
        score: Risk score
    
    Returns:
        Dictionary with all evaluation results
    """
    workable = determine_workable(status)
    dialable = determine_dialable(phone_number, phone_consent)
    risk_segment = determine_risk_segment(score)
    small_balance = determine_small_balance(balance)
    activity, dial_frequency = determine_strategy(workable, dialable, risk_segment, small_balance)
    
    return {
        'account_number': account_number,
        'workable': workable,
        'dialable': dialable,
        'risk_segment': risk_segment,
        'small_balance': small_balance,
        'activity': activity,
        'dial_frequency': dial_frequency
    }


def main():
    """
    Main entry point for command-line execution.
    Called by COBOL program with account data as arguments.
    """
    if len(sys.argv) != 7:
        print("ERROR|Invalid arguments", file=sys.stderr)
        print("Usage: python strategy_rules.py <account_number> <status> <balance> <phone> <consent> <score>", file=sys.stderr)
        sys.exit(1)
    
    try:
        account_number = sys.argv[1]
        status = sys.argv[2]
        balance = float(sys.argv[3])
        phone_number = sys.argv[4]
        phone_consent = sys.argv[5]
        score = int(sys.argv[6])
        
        result = evaluate_account(account_number, status, balance, 
                                  phone_number, phone_consent, score)
        
        output = f"{result['workable']}|{result['dialable']}|{result['risk_segment']}|{result['small_balance']}|{result['activity']}|{result['dial_frequency']}"
        print(output)
        
    except ValueError as e:
        print(f"ERROR|{str(e)}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"ERROR|{str(e)}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
