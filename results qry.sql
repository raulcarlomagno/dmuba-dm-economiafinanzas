select MAX(perfect_profit_ratio) metric, * from experiments group by plan_id order by plan_id

select MIN(logloss_testing) metric, * from experiments group by plan_id order by plan_id

select MAX(perfect_profit_ratio/logloss_testing) metric, test_periods, train_periods, *  from experiments group by plan_id order by test_periods, metric DESC

select test_periods, ROUND(perfect_profit_ratio/logloss_testing,2) metric,cutoff,  train_periods, *  from experiments order by test_periods, metric DESC



SELECT test_periods, ROUND(AVG(cutoff), 3)
from experiments
group by test_periods


SELECT h.*, e.test_periods, e.train_periods
FROM hyperparams h
INNER JOIN (select MAX(perfect_profit_ratio/logloss_testing) metric, *  from experiments group by plan_id) e on e.id = h.experiment_id
order by test_periods, metric DESC


select plan_id, id, profit, cutoff, perfect_profit_ratio, train_periods, test_periods,experiment_code
from experiments ee
where profit >= (select max(e.profit) from experiments e where e.plan_id = ee.plan_id)
group by plan_id



select * from experiments where plan_id = 381 order by profit desc
select * from hyperparams where experiment_id = 501
select * from hyperparams where experiment_id = 516
select * from hyperparams where experiment_id = 464
select * from hyperparams where experiment_id = 553
select * from hyperparams where experiment_id = 504


el experimento 654 con id 137 , puede cortar en 670 iteraciones
