

select B.id_user,
		B.diff_min,
		B.class,
		B.class2
from (

select a.*,

			case 
				when a.category not in ('top_5', 'top_5_10', 'top_10_15') then 'ordinary'
				else a.category
			end as class,
			case 
				when a.category not in ('top_5', 'top_5_10', 'top_10_15') then 'ordinary'
				else 'outstanding'
			end as class2
from time_to_first_activity('biology.stackexchange.com') a
where a.diff_min >= 0
)B 

