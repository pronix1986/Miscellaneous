-- SET TIMING ON;

WHENEVER OSERROR EXIT FAILURE;
WHENEVER SQLERROR EXIT SQL.SQLCODE;

--------------------------------------------------------------------------
-- Script parameters
--------------------------------------------------------------------------
prompt $> Enter the user/schema for the CURR schema (e.g.  YOUR_NAME_CURR)
define owner_curr=&1
prompt $> Enter the *password* for the CURR  schema (e.g. YOUR_PASSWORD_CURR)
define owner_curr_password=&2
PROMPT $> Enter THE service NAME (e.g. ELVIS)
define ov6_tns=&5
prompt $> Enter the Action plan Id
define a_p_id=&6
prompt $> Enter the Id of current reviewer(If you enter '0' then all reviewers of current Action plan will be deleted and changed at new assignee)
define curr_user_id=&7
prompt $> Enter the Id of new assignee 
define new_user_id=&8

spool reassign_reviewer.txt;

SET VERIFY OFF;
set heading off;
set serveroutput on size 1000000;
set linesize 512;
set pagesize 999;
set exitcommit off;
set feedback off;

CONNECT &owner_curr/&owner_curr_password@&ov6_tns;
	

create or replace procedure verify_users(curr_us_id number, new_us_id number) as
	type r_cursor is REF CURSOR;
	cur r_cursor;
	user_id long;
	user_name VARCHAR2(255);
	is_enabled varchar2(255);
	is_appr_role varchar2(255);
	inappropriate_user_role exception;
	no_such_user exception;
	user_primary_bu_id number;
	user_primary_bu varchar2(255);
BEGIN
	DBMS_OUTPUT.PUT_LINE(chr(10)||'***********Verifications_USERS***********');
	DBMS_OUTPUT.PUT_LINE('******');
	DBMS_OUTPUT.PUT_LINE('curr_user_id       curr_user_name       Enabled        Role_ACTIONPLAN_REVIEWER        Primary_Business_Unit');
	DBMS_OUTPUT.PUT_LINE('------------------------------------------------------------------------------------------------------------');

	open cur for select inf_obj_id, vu.username, decode(vu.enabled,  0, 'disabled', 1, 'enabled', 'unknown'), 
		nvl2(to_char((select vr.name role_name from v_role vr where vr.inf_obj_id in
		(select u.role_id from v_user_role_env u where u.user_id = vu.inf_obj_id) and vr.name = 'ACTIONPLAN_REVIEWER')), 
		'true', 'false'),
		(select vd.value_id from v_dimension vd where vd.value_type = 'PrimaryBusinessUnit' and vd.dimension_node_id in
		(select dimension_node_id from v_user_role_env where role_id = 245 and user_id = vu.inf_obj_id))
		from v_user vu
		where vu.inf_obj_id = curr_us_id;

		LOOP
			FETCH cur INTO user_id, user_name, is_enabled, is_appr_role, user_primary_bu_id;
			if(&curr_user_id <> '0' and user_name is null) then
				DBMS_OUTPUT.PUT_LINE('!!!!ERROR: Current user ('|| curr_us_id ||') does not exist');
				raise no_such_user;
			end if;
			if(user_primary_bu_id is not null) then
				select message_tx into user_primary_bu from v_i18n_message where locale_cd = 'en' and message = user_primary_bu_id;
			else 
				user_primary_bu := 'NA';
			end if;
			
			EXIT WHEN cur%notfound;
			DBMS_OUTPUT.PUT_LINE(user_id || '             ' || user_name || '             ' || is_enabled || '             ' || is_appr_role ||'                          ' || user_primary_bu );
		END LOOP;
	CLOSE cur;
	DBMS_OUTPUT.PUT_LINE('******');
	DBMS_OUTPUT.PUT_LINE('new_user_id       new_user_name        Enabled        Role_ACTIONPLAN_REVIEWER        Primary_Business_Unit');
	DBMS_OUTPUT.PUT_LINE('-----------------------------------------------------------------------------------------------------------');
	
	open cur for select inf_obj_id, vu.username, decode(vu.enabled,  0, 'disabled', 1, 'enabled', 'unknown'), 
		nvl2(to_char((select vr.name role_name from v_role vr where vr.inf_obj_id in
		(select u.role_id from v_user_role_env u where u.user_id = vu.inf_obj_id) and vr.name = 'ACTIONPLAN_REVIEWER')), 
		'true', 'false'),
		(select vd.value_id from v_dimension vd where vd.value_type = 'PrimaryBusinessUnit' and vd.dimension_node_id in
		(select dimension_node_id from v_user_role_env where role_id = 245 and user_id = vu.inf_obj_id))
		from v_user vu
		where vu.inf_obj_id = new_us_id;

		LOOP
			FETCH cur INTO user_id, user_name, is_enabled, is_appr_role, user_primary_bu_id;
			if(user_name is null) then
				DBMS_OUTPUT.PUT_LINE('!!!!ERROR: New user ('|| new_us_id ||') does not exist');
			end if;
			if(user_primary_bu_id is not null) then
				select message_tx into user_primary_bu from v_i18n_message where locale_cd = 'en' and message = user_primary_bu_id;
			else 
				user_primary_bu := 'NA';
			end if;
			EXIT WHEN cur%notfound;
			DBMS_OUTPUT.PUT_LINE(user_id || '             ' || user_name || '             ' || is_enabled || '             ' || is_appr_role ||'                          ' || user_primary_bu );
			IF (is_appr_role = 'false') THEN
				DBMS_OUTPUT.PUT_LINE('!!!!ERROR: New user ('|| new_us_id ||') should have ''ACTIONPLAN_REVIEWER'' role!');
				raise inappropriate_user_role;
			END IF;
			if(is_enabled = 'false') THEN
				dbms_output.put_line('Warning: Please, note, that new assignee is disabled by admin');
			end if;
		END LOOP;
	CLOSE cur;
END;
/
	
create or replace procedure verify_AP(ap_id number)
as
	user_id number;
	inap_ap_status exception;
	ap_not_match_user exception;
	ap_primary_bu varchar2(255);
	ap_primary_bu_id number;
	user_primary_bu_id number;
	user_dim varchar2(255);
	loop_count number :=0;
	wrong_user_count number := 0;
	
	CURSOR select_cur IS 
		SELECT t.ACTION_PLAN_ID ,t.USER_ID, u.username, va.status, tt.place_id, va.primary_business_unit
		FROM v_a_ap_a_reviewers t
		JOIN v_user u ON u.inf_obj_id  = t.user_id
		LEFT JOIN v_action_plan va ON va.inf_obj_id = t.ACTION_PLAN_ID
		LEFT JOIN t_token tt ON tt.domain_object_id = va.inf_obj_id
		WHERE t.ACTION_PLAN_ID = ap_id;

	select_rec select_cur%ROWTYPE;

BEGIN

	DBMS_OUTPUT.PUT_LINE(chr(10)||'***********Verifications_AP***********');
	DBMS_OUTPUT.PUT_LINE('******');
	DBMS_OUTPUT.PUT_LINE('ACTION_PLAN_ID  reviewer_ID/username     AP_status       place_id      Primary_Business_Unit');
	DBMS_OUTPUT.PUT_LINE('--------------------------------------------------------------------------------------------');
		
	OPEN select_cur;
		LOOP
			FETCH select_cur INTO select_rec;
			EXIT WHEN select_cur%NOTFOUND;
			loop_count := loop_count + 1;
			user_id := select_rec.USER_ID;
			ap_primary_bu_id := select_rec.primary_business_unit;
			select message_tx into ap_primary_bu from v_i18n_message where locale_cd = 'en' and message = ap_primary_bu_id;
			DBMS_OUTPUT.PUT_LINE(select_rec.ACTION_PLAN_ID ||'          '|| user_id||'/'|| select_rec.username ||'       '|| select_rec.status ||'      '|| select_rec.place_id ||'    '|| ap_primary_bu);		

			if (select_rec.status != 'PENDINGREVIEW') then
				DBMS_OUTPUT.PUT_LINE('!!!!ERROR: Action plan ('|| select_rec.ACTION_PLAN_ID ||') should be in status ''Pending Review''');
				raise inap_ap_status;
			end if;

			IF ((user_id != &curr_user_id AND &curr_user_id != '0')) THEN
				wrong_user_count := wrong_user_count + 1;
			END IF;
			
			select max(vd.value_id) into user_primary_bu_id from v_dimension vd where vd.value_type = 'PrimaryBusinessUnit' and vd.dimension_node_id in
			(select dimension_node_id from v_user_role_env where role_id = 245 and user_id = &new_user_id);
			
			select case when user_primary_bu_id not in (
				select distinct origin_node_id from v_hier_node_link
				start with destination_node_id = ap_primary_bu_id
				connect by destination_node_id = prior origin_node_id
				union select ap_primary_bu_id from dual) then 'out' else 'parent' end into user_dim from dual;
			if(user_dim = 'out') then
				DBMS_OUTPUT.PUT_LINE('!!!!ERROR: User '|| &new_user_id ||' cannot see AP with Primary Business Unit = ' || ap_primary_bu);
				raise ap_not_match_user;
			end if;
		END LOOP;
		if(loop_count = 0) then
			DBMS_OUTPUT.PUT_LINE('!!!!ERROR: Action plan '||&a_p_id ||' does not exist');
			raise ap_not_match_user;
		end if;
		if(loop_count = wrong_user_count) then
			DBMS_OUTPUT.PUT_LINE('!!!!ERROR: The Action Plan ('|| &a_p_id ||') is not assigned to user (' || &curr_user_id || ')');
			DBMS_OUTPUT.PUT_LINE('Change ''current_reviewer_id'' either on the current reviewer user, or on ''0'' value, please!');
			raise ap_not_match_user;
		end if;
		
	CLOSE select_cur;
END;
/

CREATE OR REPLACE procedure update_ap_reviewer(ap_id number, curr_us_id number, new_us_id number) as
	aplan_count int;
	update_reviewer_exception exception;
BEGIN
	INSERT INTO TMP_USER (ID, USER_ID) VALUES (1, 102);
	
	DBMS_OUTPUT.PUT_LINE(chr(10)||'*******v_a_ap_a_reviewers_begin*******');
	IF curr_us_id = 0 THEN
		delete from v_a_ap_a_reviewers where ACTION_PLAN_ID = ap_id;
		insert into v_a_ap_a_reviewers (ACTION_PLAN_ID,USER_ID) values(ap_id, new_us_id);
		DBMS_OUTPUT.PUT_LINE('*******DELETE_INSERT v_a_ap_a_reviewers for ap_id = ' || ap_id || '; new_us_id = '|| new_us_id || '*******');
	ELSE
		SELECT count(ACTION_PLAN_ID) into aplan_count FROM v_a_ap_a_reviewers WHERE ACTION_PLAN_ID = ap_id AND user_id = curr_us_id;
		IF  (aplan_count = 0) THEN
			DBMS_OUTPUT.PUT_LINE('!!!!ERROR: The Action Plan ('|| ap_id ||') does not exist for reviewer ('|| curr_us_id ||')!');
			raise update_reviewer_exception;
		ELSE
			UPDATE v_a_ap_a_reviewers SET user_id = new_us_id WHERE ACTION_PLAN_ID = ap_id AND user_id = curr_us_id;
			DBMS_OUTPUT.PUT_LINE('*******UPDATE v_a_ap_a_reviewers for ap_id = ' || ap_id || '; curr_us_id=' || curr_us_id || '; new_us_id = '|| new_us_id || '*******');
		END IF;
	END IF;
	DBMS_OUTPUT.PUT_LINE('*******v_a_ap_a_reviewers_end*******');
END;
/

CREATE OR REPLACE procedure update_token_assignees(ap_id number, curr_us_id number, new_us_id number) as
	aplan_count int;
	p_id VARCHAR2(255 BYTE);
	update_t_ass_exception exception;
BEGIN
	SELECT place_id INTO p_id FROM t_token WHERE domain_object_id = ap_id;
	
	IF p_id = 'planReview' THEN
		DBMS_OUTPUT.PUT_LINE(chr(10)||'*******t_token_assignees_begin*******');
		IF curr_us_id = 0 THEN
			delete from t_token_assignees where token_ass_id in (select inf_obj_id from (
			select inf_obj_id, row_number() over (partition by token_id order by list_order desc) rn 
			from t_token_assignment  where token_id in (select inf_obj_id from t_token where domain_object_id=ap_id ) ) where rn=1);

			insert into t_token_assignees values(new_us_id,(select inf_obj_id from (
			select inf_obj_id, row_number() over (partition by token_id order by list_order desc) rn 
			from t_token_assignment where token_id in (select inf_obj_id from t_token where domain_object_id = ap_id ) ) where rn=1 ));
			DBMS_OUTPUT.PUT_LINE('*******DELETE_INSERT t_token_assignees for ap_id = ' || ap_id || '; new_us_id = '|| new_us_id);
		ELSE
			
			SELECT count(user_id) into aplan_count FROM t_token_assignees WHERE
			token_ass_id IN (SELECT inf_obj_id FROM t_token_assignment
			WHERE token_id in (SELECT inf_obj_id FROM t_token WHERE
			domain_object_id = ap_id) AND user_id = curr_us_id);
			
			
			IF  (aplan_count = 0) THEN
				DBMS_OUTPUT.PUT_LINE('!!!!ERROR: The Action Plan ('|| ap_id ||') does not exist for reviewer ('|| curr_us_id ||')!');
				raise update_t_ass_exception;
			ELSE
				update t_token_assignees set user_id = new_us_id where
				token_ass_id in (select inf_obj_id from (
				select inf_obj_id, row_number() over (partition by token_id order by list_order desc) rn 
				from t_token_assignment where token_id in (select inf_obj_id from t_token where domain_object_id = ap_id)) 
				where rn=1) and user_id = curr_us_id;
				DBMS_OUTPUT.PUT_LINE('*******UPDATE t_token_assignees for ap_id = ' || ap_id || '; curr_us_id='|| curr_us_id || '; new_us_id = '|| new_us_id || '*******');
			END IF;
			
		END IF;
		DBMS_OUTPUT.PUT_LINE('*******t_token_assignees_end*******');
	END IF;
END;
/

declare
	
	myexception exception;

begin
	verify_users(&curr_user_id, &new_user_id);
	verify_AP(&a_p_id);
	update_ap_reviewer(&a_p_id, &curr_user_id, &new_user_id);
	update_token_assignees(&a_p_id, &curr_user_id, &new_user_id);	
	
--For testing purpose:
--	raise myexception;
	
	commit;
	DBMS_OUTPUT.PUT_LINE(chr(10)||'The script is successfully completed.'||chr(10));
exception when others then
	DBMS_OUTPUT.PUT_LINE(chr(10)||'The script was stopped due to an exception.'||chr(10));
	rollback;
end;
/

begin
	EXECUTE IMMEDIATE 'drop procedure verify_users';
	Exception when others then null;
end;
/
begin
	EXECUTE IMMEDIATE 'drop procedure verify_AP';
	Exception when others then null;
end;
/
begin
	EXECUTE IMMEDIATE 'drop procedure update_ap_reviewer';
	Exception when others then null;
end;
/
begin
	EXECUTE IMMEDIATE 'drop procedure update_token_assignees';
	Exception when others then null;
end;
/

quit;