import tableauserverclient as TSC
from jproperties import Properties
import smtplib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.mime.base import MIMEBase
from email import encoders
from datetime import datetime, timedelta
import sys

input_filename = sys.argv[3]

today = datetime.now()
if today.weekday() == 6:
    startdate_batch = int(2)
    enddate_batch = int(2)
else:
    startdate_batch = int(sys.argv[1])
    enddate_batch = int(sys.argv[2])

startdate = datetime.now().date() - timedelta(days=startdate_batch)
enddate = datetime.now().date() - timedelta(days=enddate_batch)
YYYYMMDD = datetime.strftime(startdate, '%Y%m%d')

def attach_pdf(filepath, filename):
    """
    Returns an attachment in the form of a MIMEBase object.
    """
    fp = open(filepath, 'rb')
    attachment = MIMEBase('application', 'octet-stream')
    attachment.set_payload(fp.read())
    encoders.encode_base64(attachment)
    attachment.add_header('Content-Disposition', 'attachment', filename=filename)
    return attachment

def send_email(sender, unique_user_emails_list, cc_emails, subject, body, attachments):
    sender_email = sender
    recipient_emails = list(filter(None, unique_user_emails_list or []) + (cc_emails or []))
    emailSubject = subject
    emailBody = body
    
    msg = MIMEMultipart()
    msg['From'] = sender_email
    msg['To'] = ', '.join(filter(None, unique_user_emails_list)) if unique_user_emails_list else ''
    msg['Cc'] = ', '.join(filter(None, cc_emails)) if cc_emails else ''
    msg['Subject'] = emailSubject
    
    msg.attach(MIMEText(emailBody, 'plain'))
    
    if attachments is not None:
        for attachment in attachments:
            msg.attach(attachment)
    
    text = msg.as_string()
    smtpObj = smtplib.SMTP('InternalRelay-Mail.Prudential.com', 25)
    smtpObj.sendmail(sender_email, recipient_emails, text)
    smtpObj.quit()

views_info_file_path = rf'C:\Users\pim-q-pfi-tbl-fa\AppData\Local\Programs\Python\Python311\BI_MasterCode\Japanmodifiedemail\Views_Info_Jp_Equity_Trade.txt'

Filterviews = r'C:\Users\pim-q-pfi-tbl-fa\AppData\Local\Programs\Python\Python311\BI_MasterCode\Japanmodifiedemail\Filters.txt'

# Define function
def load_properties(file_path):
    properties = Properties()
    with open(properties_file_path, 'rb') as pty_file:
        properties.load(pty_file)
    return (
        properties['tableau_server_host'].data,
        properties['tableau_server_username'].data,
        properties['tableau_server_password'].data,
        properties['tableau_server_site'].data
    )

# Path of properties file - Tableau credentials are stored
properties_file_path = r'C:\Users\pim-q-pfi-tbl-fa\AppData\Local\Programs\Python\Python311\BI_MasterCode\Japanmodifiedemail\config.properties'

# Load Tableau credentials from properties file
(
    tableau_server_host,
    tableau_server_username,
    tableau_server_password,
    tableau_server_site
) = load_properties(properties_file_path)

# To create connection to Tableau server
server = TSC.Server(tableau_server_host, use_server_version=True)
try:
    # Authentication with Tableau server
    tableau_auth = TSC.PersonalAccessTokenAuth(tableau_server_username, tableau_server_password)
    with server.auth.sign_in(tableau_auth):
        # Read the list of Projects, Workbooks, Views & output path from the workbook_views_info_file
        with open(views_info_file_path, 'r') as views_file:
            # Skipping first line from the file as this is a header
            first_line = views_file.readline()
            # The values in each line are separated by delimiter, used to read through
            views_to_export = [line.strip().split(',') for line in views_file]
        
        for ReportID, project_name, workbook_name, view_name, customviewname, Internal_group_name, Users_group_name, pdf_output_path in views_to_export:
            # Scan and check if the project is available in Tableau server
            all_project_items, pagination_item = server.projects.get()
            project_exists = any(project.name == project_name for project in all_project_items)
            
            if project_exists:
                project = next((project for project in all_project_items if project.name == project_name), None)
                print(f"Success Project : {project_name} found in Tableau server")
                # Page Size is optional, used only if workbooks count is more than 100
                request_options = TSC.RequestOptions(pagesize=1000)
                # Scan and check if the workbook is available in the Project that is retrieved above
                all_workbooks = list(TSC.Pager(server.workbooks, request_options))
                workbook_exists = any(workbook.name == workbook_name for workbook in all_workbooks)
                if workbook_exists:
                    workbook = next((workbook for workbook in all_workbooks if workbook.name == workbook_name), None)
                    print(f"Success Workbook : {workbook_name} exists in the project")
                    # Scan and check if the view is available in the workbook that is retrieved above
                    all_views = list(TSC.Pager(server.views.get, request_options))
                    view_exists = any(view.name == view_name for view in all_views if view.workbook_id == workbook.id)
                    if view_exists:
                        print(f"Success View : {view_name} exists in the workbook")
                        server.workbooks.populate_views(workbook)
                        # Get view id from the workbook
                        view_id = next((view.id for view in workbook.views if view.name == view_name), None)
                        view_item = server.views.get_by_id(view_id)
                        # Get all the groups in the server
                        all_groups, pagination_item = server.groups.get()
                        group_exists = any(group.name == Users_group_name for group in all_groups)
                        if group_exists:
                            group = next((group for group in all_groups if group.name == Users_group_name), None)
                            pagination_item = server.groups.populate_users(group)
                            # Get all the users in the group
                            unique_user_emails_list = [user.email for user in group.users]
                            print(f"emails : {unique_user_emails_list}")
                            with open(Filterviews,'r') as file:
                                dict1 = {key: value for key, *value in (line[5:].strip().split(':') for line in file if line[:4] == ReportID)}
                                # get the data field for the specific report if exists, try to replace the default values else only picks the filters that are avaliable in the report
                                print(dict1.keys())
                                date_key = any(key for key, value in dict1.items() if value[0] == '-999,111')
                                if date_key:
                                    date_key = next(key for key, value in dict1.items() if value[0] == '-999,111')
                                    remove_date_key = dict1.pop(date_key)
                                    date_range = [startdate - timedelta(days=x) for x in range((enddate-startdate).days + 1)]
                                    con_range = [dt.strftime('%Y-%m-%d') for dt in date_range]
                                    result = ','.join(con_range)
                                    dict1.update({date_key: [result]})
                                    print(dict1)
                                else:
                                    print(dict1)

                            # Set formatting and write to PDF
                            pdf_req_option = TSC.PDFRequestOptions(page_type=TSC.PDFRequestOptions.PageType.Unspecified,orientation=TSC.PDFRequestOptions.Orientation.Portrait)
                            for key, value in dict1.items():
                                pdf_req_option.vf(key, value)
                                #print(dict1) -- 5
                            pdf_content = server.views.populate_pdf(view_item, pdf_req_option)
                            pdf_path = f"{customviewname}_{YYYYMMDD}.pdf"
                            with open(pdf_path.format(view_item.name), 'wb') as f:
                                f.write(view_item.pdf)
                            print(f"PDF exported successfully: {pdf_path}")
                            attachment = attach_pdf(f"{pdf_output_path}\{customviewname}_{YYYYMMDD}.pdf", f"{customviewname}_{YYYYMMDD}.pdf")
                            #send_email('Tableau_QAUtility.donotreply@prudential.com',unique_user_emails_list,f'{view_name} + Job execution Success', Exported PDF Successfully, [attachment])
                            send_email('Tableau_QAUtility.donotreply@prudential.com', [], ['pgim.fi.tsg.business.intelligence@prudential.com'], f'{customviewname} + Job execution Success', f'{customviewname} + Job execution Success', [attachment])
                            print(f"Success Mail : {send_email}")
                        else:
                            print(f"Error_Group : {Users_group_name} not available")
                            send_email('Tableau_QAUtility.donotreply@prudential.com', [], ['charlotte.wang@pgim.com'], f'{customviewname} + Job execution Failed', f'{customviewname} + Job execution Failed', None)
                    else:
                        print(f"Error_View : {view_name} not found in {workbook_name} workbook")
                        send_email('Tableau_QAUtility.donotreply@prudential.com', [], ['charlotte.wang@pgim.com'], f'{customviewname} + Job execution Failed', f'{customviewname} + Job execution Failed', None)
                else:
                    print(f"Error_Workbook : {workbook_name} not found in {project_name} project")
                    send_email('Tableau_QAUtility.donotreply@prudential.com', [], ['charlotte.wang@pgim.com'], f'{customviewname} + Job execution Failed', f'{customviewname} + Job execution Failed', None)
            else:
                print(f"Error_Project : {project_name} project not found in Tableau Server")
                send_email('Tableau_QAUtility.donotreply@prudential.com', [], ['charlotte.wang@pgim.com'], f'{customviewname} + Job execution Failed', f'{customviewname} + Job execution Failed', None)
                server.auth.sign_out()
except Exception as e:
    send_email('Tableau_QAUtility.donotreply@prudential.com', [], ['charlotte.wang@pgim.com'], f'{customviewname} + Job execution Failed', f"Error : {str(e)}", None)

