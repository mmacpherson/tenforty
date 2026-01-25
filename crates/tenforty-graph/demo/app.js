import init, { FilingStatus, Graph, Runtime } from './pkg/graphlib.js';

let graph = null;
let runtime = null;
let taxCurveData = [];
let stackedCurveData = [];

const CHART_PADDING = { top: 20, right: 20, bottom: 40, left: 60 };

const TAX_COMPONENTS = [
    { key: 'ordinary', label: 'Ordinary Income', color: '#3498db' },
    { key: 'ltcg', label: 'Capital Gains', color: '#9b59b6' },
    { key: 'amt', label: 'AMT', color: '#e74c3c' },
    { key: 'niit', label: 'NIIT', color: '#e67e22' },
    { key: 'medicare', label: 'Add\'l Medicare', color: '#f1c40f' },
    { key: 'state', label: 'State', color: '#1abc9c' }
];
const CHART_WIDTH = 600;
const CHART_HEIGHT = 300;

const BRACKET_COLORS = ['#2ecc71', '#27ae60', '#f1c40f', '#e67e22', '#e74c3c', '#c0392b', '#8e44ad'];
const BRACKET_RATES = [0.10, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37];

function formatCurrency(value) {
    return new Intl.NumberFormat('en-US', {
        style: 'currency',
        currency: 'USD',
        minimumFractionDigits: 0,
        maximumFractionDigits: 0
    }).format(value);
}

function formatPercent(value) {
    return value.toFixed(1) + '%';
}

function getFilingStatus(value) {
    switch (value) {
        case 'single': return FilingStatus.single();
        case 'married_joint': return FilingStatus.married_joint();
        case 'married_separate': return FilingStatus.married_separate();
        case 'head_of_household': return FilingStatus.head_of_household();
        case 'qualifying_widow': return FilingStatus.qualifying_widow();
        default: return FilingStatus.single();
    }
}

function getBrackets(filingStatus) {
    const brackets = {
        'single': [
            { threshold: 11600, rate: 0.10 },
            { threshold: 47150, rate: 0.12 },
            { threshold: 100525, rate: 0.22 },
            { threshold: 191950, rate: 0.24 },
            { threshold: 243725, rate: 0.32 },
            { threshold: 609350, rate: 0.35 },
            { threshold: Infinity, rate: 0.37 }
        ],
        'married_joint': [
            { threshold: 23200, rate: 0.10 },
            { threshold: 94300, rate: 0.12 },
            { threshold: 201050, rate: 0.22 },
            { threshold: 383900, rate: 0.24 },
            { threshold: 487450, rate: 0.32 },
            { threshold: 731200, rate: 0.35 },
            { threshold: Infinity, rate: 0.37 }
        ],
        'married_separate': [
            { threshold: 11600, rate: 0.10 },
            { threshold: 47150, rate: 0.12 },
            { threshold: 100525, rate: 0.22 },
            { threshold: 191950, rate: 0.24 },
            { threshold: 243725, rate: 0.32 },
            { threshold: 365600, rate: 0.35 },
            { threshold: Infinity, rate: 0.37 }
        ],
        'head_of_household': [
            { threshold: 16550, rate: 0.10 },
            { threshold: 63100, rate: 0.12 },
            { threshold: 100500, rate: 0.22 },
            { threshold: 191950, rate: 0.24 },
            { threshold: 243700, rate: 0.32 },
            { threshold: 609350, rate: 0.35 },
            { threshold: Infinity, rate: 0.37 }
        ],
        'qualifying_widow': [
            { threshold: 23200, rate: 0.10 },
            { threshold: 94300, rate: 0.12 },
            { threshold: 201050, rate: 0.22 },
            { threshold: 383900, rate: 0.24 },
            { threshold: 487450, rate: 0.32 },
            { threshold: 731200, rate: 0.35 },
            { threshold: Infinity, rate: 0.37 }
        ]
    };
    return brackets[filingStatus] || brackets['single'];
}

function createRuntime() {
    const statusValue = document.getElementById('filing-status').value;
    const status = getFilingStatus(statusValue);
    runtime = new Runtime(graph, status);
}

function getInputValues() {
    return {
        wages: parseFloat(document.getElementById('wages').value) || 0,
        interest: parseFloat(document.getElementById('interest').value) || 0,
        dividends: parseFloat(document.getElementById('dividends').value) || 0,
        stcg: parseFloat(document.getElementById('stcg').value) || 0,
        ltcg: parseFloat(document.getElementById('ltcg').value) || 0,
        iso: parseFloat(document.getElementById('iso').value) || 0
    };
}

function setRuntimeInputs(values) {
    runtime.set('wages', values.wages);
    runtime.set('interest', values.interest);
    runtime.set('dividends', values.dividends);
    runtime.set('short_term_capital_gains', values.stcg);
    runtime.set('long_term_capital_gains', values.ltcg);
    runtime.set('iso_exercise_gains', values.iso);
}

function safeEval(nodeName) {
    try {
        const val = runtime.eval(nodeName);
        return isFinite(val) ? val : 0;
    } catch (e) {
        return 0;
    }
}

function safeGradient(output, input) {
    try {
        const val = runtime.gradient(output, input);
        return isFinite(val) ? val : 0;
    } catch (e) {
        return 0;
    }
}

function updateResults() {
    if (!graph || !runtime) return;

    const values = getInputValues();
    const showState = document.getElementById('state').value === 'california';

    try {
        setRuntimeInputs(values);

        const grossIncome = safeEval('gross_income');
        const stdDeduction = safeEval('standard_deduction');
        const taxableIncome = safeEval('taxable_ordinary_income');
        const ordinaryTax = safeEval('ordinary_income_tax');
        const ltcgTax = safeEval('ltcg_tax');
        const niit = safeEval('niit');
        const medicareTax = safeEval('additional_medicare_tax');
        const amt = safeEval('amt');
        const federalTax = safeEval('federal_tax');
        const stateTax = safeEval('ca_state_tax');
        const totalTax = safeEval('total_tax');
        const effectiveRate = grossIncome > 0 ? safeEval('federal_effective_rate') : 0;
        const marginalRate = safeGradient('federal_tax', 'wages') * 100;

        document.getElementById('gross-income').textContent = formatCurrency(grossIncome);
        document.getElementById('std-deduction').textContent = '-' + formatCurrency(stdDeduction);
        document.getElementById('taxable-income').textContent = formatCurrency(taxableIncome);
        document.getElementById('ordinary-tax').textContent = formatCurrency(ordinaryTax);
        document.getElementById('ltcg-tax').textContent = formatCurrency(ltcgTax);

        const ltcgRow = document.getElementById('ltcg-tax-row');
        ltcgRow.style.display = ltcgTax > 0 ? 'flex' : 'none';

        const niitRow = document.getElementById('niit-row');
        niitRow.style.display = niit > 0 ? 'flex' : 'none';
        document.getElementById('niit').textContent = formatCurrency(niit);

        const medicareRow = document.getElementById('medicare-row');
        medicareRow.style.display = medicareTax > 0 ? 'flex' : 'none';
        document.getElementById('medicare-tax').textContent = formatCurrency(medicareTax);

        const federalBeforeAmt = safeEval('federal_before_amt');
        const tentativeMinTax = safeEval('tentative_minimum_tax');
        const amtCushion = federalBeforeAmt - tentativeMinTax;

        const amtRow = document.getElementById('amt-row');
        const amtLabel = document.getElementById('amt-label');
        const amtValue = document.getElementById('amt');

        if (amt > 0) {
            amtRow.style.display = 'flex';
            amtRow.classList.add('in-amt');
            amtRow.classList.remove('amt-cushion');
            amtLabel.textContent = 'AMT (paying extra)';
            amtValue.textContent = '+' + formatCurrency(amt);
        } else if (values.iso > 0 && amtCushion > 0) {
            amtRow.style.display = 'flex';
            amtRow.classList.remove('in-amt');
            amtRow.classList.add('amt-cushion');
            amtLabel.textContent = 'AMT Cushion';
            amtValue.textContent = formatCurrency(amtCushion);
        } else {
            amtRow.style.display = 'none';
        }

        document.getElementById('federal-tax').textContent = formatCurrency(federalTax);

        const stateRow = document.getElementById('state-tax-row');
        const totalRow = document.getElementById('total-row');
        stateRow.style.display = showState ? 'flex' : 'none';
        totalRow.style.display = showState ? 'flex' : 'none';
        document.getElementById('state-tax').textContent = formatCurrency(stateTax);
        document.getElementById('total-tax').textContent = formatCurrency(showState ? totalTax : federalTax);

        document.getElementById('effective-rate').textContent = formatPercent(effectiveRate);
        document.getElementById('marginal-rate').textContent = formatPercent(marginalRate);

        updateSensitivity();
        updateBracketVisualization(taxableIncome);
        const isStacked = document.getElementById('stacked-toggle').checked;
        const chartTax = (showState && isStacked) ? totalTax : federalTax;
        updateTaxCurve(values.wages, chartTax);
    } catch (e) {
        console.error('Evaluation error:', e);
    }
}

function updateSensitivity() {
    const showState = document.getElementById('state').value === 'california';
    const taxNode = showState ? 'total_tax' : 'federal_tax';

    const wagesGrad = safeGradient(taxNode, 'wages');
    const interestGrad = safeGradient(taxNode, 'interest');
    const ltcgGrad = safeGradient(taxNode, 'long_term_capital_gains');

    document.getElementById('sens-wages').textContent = '+' + formatCurrency(wagesGrad * 1000);
    document.getElementById('sens-wages-rate').textContent = formatPercent(wagesGrad * 100);

    document.getElementById('sens-interest').textContent = '+' + formatCurrency(interestGrad * 1000);
    document.getElementById('sens-interest-rate').textContent = formatPercent(interestGrad * 100);

    document.getElementById('sens-ltcg').textContent = '+' + formatCurrency(ltcgGrad * 1000);
    document.getElementById('sens-ltcg-rate').textContent = formatPercent(ltcgGrad * 100);
}

function updateBracketVisualization(taxableIncome) {
    const filingStatus = document.getElementById('filing-status').value;
    const brackets = getBrackets(filingStatus);
    const bracketBar = document.getElementById('bracket-bar');
    const bracketLegend = document.getElementById('bracket-legend');

    const maxIncome = Math.max(taxableIncome * 1.5, 250000);
    let prevThreshold = 0;
    let currentBracketIndex = -1;

    bracketBar.innerHTML = '';
    bracketLegend.innerHTML = '';

    brackets.forEach((bracket, i) => {
        const bracketEnd = Math.min(bracket.threshold, maxIncome);
        const bracketWidth = bracketEnd - prevThreshold;

        if (bracketWidth <= 0) return;

        const widthPercent = (bracketWidth / maxIncome) * 100;
        const segment = document.createElement('div');
        segment.className = 'bracket-segment';
        segment.style.flex = widthPercent;
        segment.textContent = (bracket.rate * 100) + '%';

        if (taxableIncome > prevThreshold && taxableIncome <= bracket.threshold) {
            currentBracketIndex = i;
            segment.classList.add('bracket-marker');
        }

        bracketBar.appendChild(segment);

        const legendItem = document.createElement('div');
        legendItem.className = 'legend-item' + (i === currentBracketIndex ? ' current' : '');
        legendItem.innerHTML = `
            <span class="legend-color" style="background: ${BRACKET_COLORS[i]}"></span>
            <span>${(bracket.rate * 100)}%: ${formatCurrency(prevThreshold)} - ${bracket.threshold === Infinity ? 'above' : formatCurrency(bracket.threshold)}</span>
        `;
        bracketLegend.appendChild(legendItem);

        prevThreshold = bracket.threshold;
    });
}

function computeTaxCurve(overrideValues = null) {
    const numPoints = 100;
    const maxIncome = 500000;
    const values = overrideValues || getInputValues();
    const showState = document.getElementById('state').value === 'california';

    console.log('Computing curve with ISO:', values.iso, 'LTCG:', values.ltcg);

    taxCurveData = [];
    stackedCurveData = [];

    for (let i = 0; i <= numPoints; i++) {
        const income = (i / numPoints) * maxIncome;
        runtime.set('wages', income);
        runtime.set('interest', values.interest);
        runtime.set('dividends', values.dividends);
        runtime.set('short_term_capital_gains', values.stcg);
        runtime.set('long_term_capital_gains', values.ltcg);
        runtime.set('iso_exercise_gains', values.iso);

        const tax = safeEval('federal_tax');
        taxCurveData.push({ income, tax });

        stackedCurveData.push({
            income,
            ordinary: safeEval('ordinary_income_tax'),
            ltcg: safeEval('ltcg_tax'),
            amt: safeEval('amt'),
            niit: safeEval('niit'),
            medicare: safeEval('additional_medicare_tax'),
            state: showState ? safeEval('ca_state_tax') : 0
        });
    }

    console.log('Curve at wages=0:', taxCurveData[0].tax, 'at wages=200K:', taxCurveData[40].tax);

    setRuntimeInputs(values);
}

function updateTaxCurve(currentIncome, currentTax) {
    const svg = document.getElementById('tax-chart');
    const width = CHART_WIDTH - CHART_PADDING.left - CHART_PADDING.right;
    const height = CHART_HEIGHT - CHART_PADDING.top - CHART_PADDING.bottom;
    const isStacked = document.getElementById('stacked-toggle').checked;
    const showState = document.getElementById('state').value === 'california';

    if (taxCurveData.length === 0) {
        computeTaxCurve();
    }

    const maxIncome = 500000;

    // For stacked, compute max as sum of all components; otherwise use total tax
    let maxTax;
    if (isStacked && stackedCurveData.length > 0) {
        const componentKeys = TAX_COMPONENTS
            .filter(c => c.key !== 'state' || showState)
            .map(c => c.key);
        maxTax = Math.max(...stackedCurveData.map(d =>
            componentKeys.reduce((sum, key) => sum + (d[key] || 0), 0)
        ), currentTax) * 1.1;
    } else {
        maxTax = Math.max(...taxCurveData.map(d => d.tax), currentTax) * 1.1;
    }

    const xScale = (val) => CHART_PADDING.left + (val / maxIncome) * width;
    const yScale = (val) => CHART_PADDING.top + height - (val / maxTax) * height;

    // Draw grid
    const gridGroup = svg.querySelector('.grid');
    gridGroup.innerHTML = '';

    for (let i = 0; i <= 5; i++) {
        const y = CHART_PADDING.top + (i / 5) * height;
        const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
        line.setAttribute('x1', CHART_PADDING.left);
        line.setAttribute('x2', CHART_WIDTH - CHART_PADDING.right);
        line.setAttribute('y1', y);
        line.setAttribute('y2', y);
        gridGroup.appendChild(line);
    }

    for (let i = 0; i <= 5; i++) {
        const x = CHART_PADDING.left + (i / 5) * width;
        const line = document.createElementNS('http://www.w3.org/2000/svg', 'line');
        line.setAttribute('x1', x);
        line.setAttribute('x2', x);
        line.setAttribute('y1', CHART_PADDING.top);
        line.setAttribute('y2', CHART_HEIGHT - CHART_PADDING.bottom);
        gridGroup.appendChild(line);
    }

    // Draw axis labels
    const labelsGroup = svg.querySelector('.axis-labels');
    labelsGroup.innerHTML = '';

    for (let i = 0; i <= 5; i++) {
        const income = (i / 5) * maxIncome;
        const x = xScale(income);
        const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
        text.setAttribute('x', x);
        text.setAttribute('y', CHART_HEIGHT - 18);
        text.setAttribute('text-anchor', 'middle');
        text.textContent = '$' + (income / 1000) + 'K';
        labelsGroup.appendChild(text);
    }

    // X axis label
    const xLabel = document.createElementNS('http://www.w3.org/2000/svg', 'text');
    xLabel.setAttribute('x', CHART_PADDING.left + width / 2);
    xLabel.setAttribute('y', CHART_HEIGHT - 2);
    xLabel.setAttribute('text-anchor', 'middle');
    xLabel.setAttribute('font-size', '11');
    xLabel.setAttribute('fill', '#7f8c8d');
    xLabel.textContent = 'Wages';
    labelsGroup.appendChild(xLabel);

    for (let i = 0; i <= 4; i++) {
        const tax = ((4 - i) / 4) * maxTax;
        const y = CHART_PADDING.top + (i / 4) * height;
        const text = document.createElementNS('http://www.w3.org/2000/svg', 'text');
        text.setAttribute('x', CHART_PADDING.left - 5);
        text.setAttribute('y', y + 4);
        text.setAttribute('text-anchor', 'end');
        text.textContent = '$' + Math.round(tax / 1000) + 'K';
        labelsGroup.appendChild(text);
    }

    // Handle stacked vs simple mode
    const stackedGroup = svg.querySelector('.stacked-areas');
    const simpleCurve = svg.querySelector('.tax-curve');
    const simpleArea = svg.querySelector('.tax-area');
    const legendDiv = document.getElementById('stacked-legend');

    stackedGroup.innerHTML = '';

    if (isStacked && stackedCurveData.length > 0) {
        // Hide simple curve, show stacked
        simpleCurve.style.display = 'none';
        simpleArea.style.display = 'none';
        legendDiv.style.display = 'flex';

        // Filter components to show (exclude state if not selected)
        const activeComponents = TAX_COMPONENTS.filter(c => c.key !== 'state' || showState);

        // Build cumulative stacks
        const stacks = activeComponents.map((comp, idx) => {
            return stackedCurveData.map((d, i) => {
                const baseY = activeComponents.slice(0, idx).reduce((sum, c) => sum + (d[c.key] || 0), 0);
                const topY = baseY + (d[comp.key] || 0);
                return { income: d.income, baseY, topY };
            });
        });

        // Draw stacked areas (bottom to top)
        stacks.forEach((stack, idx) => {
            const comp = activeComponents[idx];
            let areaPath = `M ${xScale(0)} ${yScale(stack[0].baseY)}`;

            // Top edge (left to right)
            stack.forEach(pt => {
                areaPath += ` L ${xScale(pt.income)} ${yScale(pt.topY)}`;
            });

            // Bottom edge (right to left)
            for (let i = stack.length - 1; i >= 0; i--) {
                areaPath += ` L ${xScale(stack[i].income)} ${yScale(stack[i].baseY)}`;
            }
            areaPath += ' Z';

            const path = document.createElementNS('http://www.w3.org/2000/svg', 'path');
            path.setAttribute('d', areaPath);
            path.setAttribute('fill', comp.color);
            stackedGroup.appendChild(path);
        });

        // Update legend
        legendDiv.innerHTML = activeComponents.map(c =>
            `<div class="stacked-legend-item">
                <span class="stacked-legend-color" style="background: ${c.color}"></span>
                <span>${c.label}</span>
            </div>`
        ).join('');
    } else {
        // Show simple curve
        simpleCurve.style.display = '';
        simpleArea.style.display = '';
        legendDiv.style.display = 'none';

        let pathD = '';
        let areaD = `M ${xScale(0)} ${yScale(0)} `;

        taxCurveData.forEach((point, i) => {
            const x = xScale(point.income);
            const y = yScale(point.tax);
            if (i === 0) {
                pathD = `M ${x} ${y}`;
                areaD += `L ${x} ${y} `;
            } else {
                pathD += ` L ${x} ${y}`;
                areaD += `L ${x} ${y} `;
            }
        });

        areaD += `L ${xScale(maxIncome)} ${yScale(0)} Z`;

        simpleCurve.setAttribute('d', pathD);
        simpleArea.setAttribute('d', areaD);
    }

    // Position current point
    const currentX = xScale(Math.min(currentIncome, maxIncome));
    const currentY = yScale(currentTax);

    const pointGroup = svg.querySelector('.current-point');
    const circle = pointGroup.querySelector('circle');
    const crosshairV = pointGroup.querySelector('.crosshair-v');
    const crosshairH = pointGroup.querySelector('.crosshair-h');

    circle.setAttribute('cx', currentX);
    circle.setAttribute('cy', currentY);

    crosshairV.setAttribute('x1', currentX);
    crosshairV.setAttribute('x2', currentX);
    crosshairV.setAttribute('y1', CHART_PADDING.top);
    crosshairV.setAttribute('y2', CHART_HEIGHT - CHART_PADDING.bottom);

    crosshairH.setAttribute('x1', CHART_PADDING.left);
    crosshairH.setAttribute('x2', CHART_WIDTH - CHART_PADDING.right);
    crosshairH.setAttribute('y1', currentY);
    crosshairH.setAttribute('y2', currentY);
}

function handleSolve() {
    if (!graph || !runtime) return;

    const targetTax = parseFloat(document.getElementById('target-tax').value) || 0;
    const values = getInputValues();

    const btn = document.getElementById('solve-btn');
    const btnText = btn.querySelector('.btn-text');
    const btnLoading = btn.querySelector('.btn-loading');
    const animationDiv = document.getElementById('solver-animation');
    const resultDiv = document.getElementById('solver-result');

    btn.disabled = true;
    btnText.style.display = 'none';
    btnLoading.style.display = 'inline';
    animationDiv.style.display = 'block';
    resultDiv.style.display = 'none';

    runtime.set('interest', values.interest);
    runtime.set('dividends', values.dividends);
    runtime.set('short_term_capital_gains', values.stcg);
    runtime.set('long_term_capital_gains', values.ltcg);
    runtime.set('iso_exercise_gains', values.iso);

    animateSolver(targetTax, values.wages).then(result => {
        btn.disabled = false;
        btnText.style.display = 'inline';
        btnLoading.style.display = 'none';

        if (result.success) {
            document.getElementById('solved-wages').textContent = formatCurrency(result.wages);
            resultDiv.style.display = 'flex';
        } else {
            document.getElementById('solved-wages').textContent = 'Could not converge';
            resultDiv.style.display = 'flex';
        }
    });
}

async function animateSolver(targetTax, initialGuess) {
    const svg = document.getElementById('solver-chart');
    const iterCountEl = document.getElementById('iter-count');
    const iterErrorEl = document.getElementById('iter-error');

    const minWages = 0;
    const maxWages = Math.max(initialGuess * 3, 300000);
    const padding = { left: 40, right: 20, top: 15, bottom: 25 };
    const width = 400 - padding.left - padding.right;
    const height = 200 - padding.top - padding.bottom;

    const curvePoints = [];
    for (let i = 0; i <= 50; i++) {
        const w = (i / 50) * maxWages;
        runtime.set('wages', w);
        const tax = safeEval('federal_tax');
        curvePoints.push({ wages: w, tax });
    }

    const maxTax = Math.max(...curvePoints.map(p => p.tax), targetTax) * 1.1;

    const xScale = (w) => padding.left + (w / maxWages) * width;
    const yScale = (t) => padding.top + height - (t / maxTax) * height;

    let curveD = '';
    curvePoints.forEach((p, i) => {
        const x = xScale(p.wages);
        const y = yScale(p.tax);
        curveD += (i === 0 ? 'M' : 'L') + ` ${x} ${y} `;
    });

    svg.querySelector('.solver-curve').setAttribute('d', curveD);

    const targetLine = svg.querySelector('.target-line');
    targetLine.setAttribute('x1', padding.left);
    targetLine.setAttribute('x2', 400 - padding.right);
    targetLine.setAttribute('y1', yScale(targetTax));
    targetLine.setAttribute('y2', yScale(targetTax));

    svg.querySelector('.iteration-points').innerHTML = '';

    const iterations = [];
    let currentWages = initialGuess;
    const maxIter = 20;
    const tolerance = 1;

    for (let iter = 0; iter < maxIter; iter++) {
        runtime.set('wages', currentWages);
        const tax = safeEval('federal_tax');
        const gradient = safeGradient('federal_tax', 'wages');
        const error = tax - targetTax;

        iterations.push({
            wages: currentWages,
            tax,
            gradient,
            error
        });

        if (Math.abs(error) < tolerance || gradient === 0) {
            break;
        }

        currentWages = currentWages - error / gradient;
        currentWages = Math.max(0, Math.min(currentWages, maxWages * 2));
    }

    for (let i = 0; i < iterations.length; i++) {
        const iter = iterations[i];

        await new Promise(r => setTimeout(r, 300));

        const x = xScale(iter.wages);
        const y = yScale(iter.tax);

        const pointsGroup = svg.querySelector('.iteration-points');
        const circle = document.createElementNS('http://www.w3.org/2000/svg', 'circle');
        circle.setAttribute('cx', x);
        circle.setAttribute('cy', y);
        circle.setAttribute('r', 4);
        pointsGroup.appendChild(circle);

        const currentCircle = svg.querySelector('.current-iter');
        currentCircle.setAttribute('cx', x);
        currentCircle.setAttribute('cy', y);

        if (iter.gradient !== 0 && i < iterations.length - 1) {
            const nextWages = iter.wages - iter.error / iter.gradient;
            const tangentX1 = Math.max(xScale(iter.wages - 20000), padding.left);
            const tangentX2 = Math.min(xScale(iter.wages + 20000), 400 - padding.right);
            const tangentY1 = y - (tangentX1 - x) * (-iter.gradient * (maxWages / width) / (maxTax / height));
            const tangentY2 = y - (tangentX2 - x) * (-iter.gradient * (maxWages / width) / (maxTax / height));

            const tangentLine = svg.querySelector('.tangent-line');
            tangentLine.setAttribute('x1', tangentX1);
            tangentLine.setAttribute('y1', tangentY1);
            tangentLine.setAttribute('x2', tangentX2);
            tangentLine.setAttribute('y2', tangentY2);
        }

        iterCountEl.textContent = i + 1;
        iterErrorEl.textContent = formatCurrency(Math.abs(iter.error));
    }

    const finalIter = iterations[iterations.length - 1];
    return {
        success: Math.abs(finalIter.error) < 100,
        wages: finalIter.wages
    };
}

function syncSliders() {
    const sliderPairs = [
        ['wages-slider', 'wages'],
        ['interest-slider', 'interest'],
        ['dividends-slider', 'dividends'],
        ['stcg-slider', 'stcg'],
        ['ltcg-slider', 'ltcg'],
        ['iso-slider', 'iso']
    ];

    sliderPairs.forEach(([sliderId, inputId]) => {
        const slider = document.getElementById(sliderId);
        const input = document.getElementById(inputId);
        const invalidatesCurve = inputId !== 'wages';

        slider.addEventListener('input', () => {
            input.value = slider.value;
            if (invalidatesCurve) {
                taxCurveData = [];
                computeTaxCurve(getInputValues());
            }
            updateResults();
        });

        input.addEventListener('input', () => {
            slider.value = Math.min(input.value, slider.max);
            if (invalidatesCurve) {
                taxCurveData = [];
                computeTaxCurve(getInputValues());
            }
            updateResults();
        });
    });
}

function setupCollapsibles() {
    document.querySelectorAll('.collapsible').forEach(section => {
        const header = section.querySelector('.collapsible-header');
        header.addEventListener('click', () => {
            section.classList.toggle('open');
        });
    });
}

async function loadGraph() {
    const response = await fetch('graph_source.json');
    const json = await response.text();
    graph = Graph.fromJson(json);
}

async function main() {
    try {
        await init();
        await loadGraph();

        createRuntime();
        syncSliders();
        setupCollapsibles();
        computeTaxCurve();
        updateResults();

        document.getElementById('filing-status').addEventListener('change', () => {
            createRuntime();
            taxCurveData = [];
            computeTaxCurve();
            updateResults();
        });

        document.getElementById('state').addEventListener('change', () => {
            taxCurveData = [];
            computeTaxCurve();
            updateResults();
        });

        document.getElementById('stacked-toggle').addEventListener('change', updateResults);

        document.getElementById('solve-btn').addEventListener('click', handleSolve);
        document.getElementById('target-tax').addEventListener('keypress', (e) => {
            if (e.key === 'Enter') handleSolve();
        });

        document.getElementById('loading').style.display = 'none';
    } catch (e) {
        console.error('Initialization error:', e);
        document.getElementById('loading').innerHTML = `
            <p style="color: #e74c3c;">Failed to load: ${e.message}</p>
            <p>Make sure to run: <code>make wasm-dev</code></p>
        `;
    }
}

main();
