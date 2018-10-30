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

