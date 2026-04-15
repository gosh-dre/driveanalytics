import pandas as pd
import numpy as np 
import plotly.graph_objects as go
from typing import Dict, Tuple

def plot_ckd_to_cvd(row: pd.Series) -> int:
    # Check for missing values
    if pd.isna(row.get('Troponin')) or pd.isna(row.get('eGFR')) or pd.isna(row.get('BNP')):
        return np.nan
        
    if row.get('Troponin', 0) > 0.04:
        return 1
    elif row.get('eGFR', 100) < 30:
        return 1
    elif row.get('BNP', 0) > 100:
        return 1
    return 0

def assess_ckd_risk(df_ckd: pd.DataFrame, threshold_value: float = None) -> Dict:

    if df_ckd is None or len(df_ckd) == 0:
        return {
            'df': None,
            'plots': {},
            'summary_stats': None 
        }

    data = df_ckd.copy()
    
    data['Risk'] = data.apply(plot_ckd_to_cvd, axis=1)
    
    data['Risk_Label'] = data['Risk'].map({
        1: 'High Risk',
        0: 'Low Risk'
    })

    summary_stats = data.groupby('Risk_Label').agg({   
        'Troponin': ['mean', 'min', 'max'],
        'BNP': ['mean', 'min', 'max'],
        'eGFR': ['mean', 'min', 'max'],
        'Risk': 'count'
    }).round(2)
    
    summary_stats.columns = ['_'.join(col).strip() for col in summary_stats.columns.values]
    summary_stats = summary_stats.rename(columns={'Risk_count': 'Patient_Count'})

    # Generate plotly plots as JSON
    plots = generate_risk_plots(data)
    
    return {
        'df': data,
        'plots': plots,
        'summary_stats': summary_stats,
        'risk_counts': data['Risk_Label'].value_counts()
    }


def generate_risk_plots(data: pd.DataFrame) -> Dict:
    """
    Generate visualization plots for risk assessment using plotly,
    and return them as JSON strings for R/Shiny integration.
    """
    plot_json = {}
    
    # Plot 1: Risk Distribution Bar Chart
    risk_counts = data['Risk_Label'].value_counts()
    colors = ['#2ecc71' if label == 'Low Risk' else '#e74c3c' 
              for label in risk_counts.index.tolist()]
    
    fig1 = go.Figure(data=[
        go.Bar(
            x=risk_counts.index.tolist(),
            y=risk_counts.values.tolist(),
            marker_color=colors,
            text=risk_counts.values.tolist(),
            textposition='auto'
        )
    ])
    
    fig1.update_layout(
        title='Distribution of High Risk vs Low Risk Patients',
        xaxis_title='Risk Category',
        yaxis_title='Count of Patients',
        plot_bgcolor='white',
        paper_bgcolor='white',
        showlegend=False
    )
    
    plot_json['risk_distribution'] = fig1.to_json()
    
    # Plot 2: Scatter - eGFR vs Troponin
    fig2 = go.Figure()
    
    low_risk = data[data['Risk'] == 0]
    high_risk = data[data['Risk'] == 1]
    
    if len(low_risk) > 0:
        fig2.add_trace(go.Scatter(
            x=low_risk['eGFR'].tolist(),
            y=low_risk['Troponin'].tolist(),
            mode='markers',
            name='Low Risk',
            marker=dict(size=10, color='#2ecc71', opacity=0.6)
        ))
    
    if len(high_risk) > 0:
        fig2.add_trace(go.Scatter(
            x=high_risk['eGFR'].tolist(),
            y=high_risk['Troponin'].tolist(),
            mode='markers',
            name='High Risk',
            marker=dict(size=10, color='#e74c3c', opacity=0.6)
        ))
    
    # Add threshold lines
    fig2.add_hline(y=0.04, line_dash="dash", line_color="orange", 
                   annotation_text="Troponin Threshold (0.04)")
    fig2.add_vline(x=30, line_dash="dash", line_color="blue", 
                   annotation_text="eGFR Threshold (30)")
    
    fig2.update_layout(
        title='Risk Assessment: eGFR vs Troponin',
        xaxis_title='eGFR (Kidney Function)',
        yaxis_title='Troponin (Heart Damage)',
        plot_bgcolor='white',
        paper_bgcolor='white',
        hovermode='closest'
    )
    
    plot_json['egfr_troponin'] = fig2.to_json()
    
    # Plot 3: Scatter - eGFR vs BNP
    fig3 = go.Figure()
    
    if len(low_risk) > 0:
        fig3.add_trace(go.Scatter(
            x=low_risk['eGFR'].tolist(),
            y=low_risk['BNP'].tolist(),
            mode='markers',
            name='Low Risk',
            marker=dict(size=10, color='#2ecc71', opacity=0.6)
        ))
    
    if len(high_risk) > 0:
        fig3.add_trace(go.Scatter(
            x=high_risk['eGFR'].tolist(),
            y=high_risk['BNP'].tolist(),
            mode='markers',
            name='High Risk',
            marker=dict(size=10, color='#e74c3c', opacity=0.6)
        ))
    
    # Add threshold lines
    fig3.add_hline(y=100, line_dash="dash", line_color="orange", 
                   annotation_text="BNP Threshold (100)")
    fig3.add_vline(x=30, line_dash="dash", line_color="blue", 
                   annotation_text="eGFR Threshold (30)")
    
    fig3.update_layout(
        title='Risk Assessment: eGFR vs BNP',
        xaxis_title='eGFR (Kidney Function)',
        yaxis_title='BNP (Heart Stress)',
        plot_bgcolor='white',
        paper_bgcolor='white',
        hovermode='closest'
    )
    
    plot_json['egfr_bnp'] = fig3.to_json()
    
    return plot_json


