/**
 * School Management System - Real-world Pakistani Dummy Data Generator
 * Description: Generates a highly realistic, noisy dataset of 300 students.
 *              Includes failures, absentees, realistic grade distribution,
 *              and intentional duplicate records for data cleaning simulation.
 * Output: students.json, students.csv
 */

const fs = require('fs');
const path = require('path');

// ==========================================
// 1. CONSTANTS & POOLS (Same as before but expanded)
// ==========================================

const CITIES_DATA = [
    { city: 'Karachi', province: 'Sindh', postalCode: '74200', areas: ['Gulshan-e-Iqbal', 'Clifton', 'Defense (DHA)', 'North Nazimabad', 'PECHS'] },
    { city: 'Lahore', province: 'Punjab', postalCode: '54000', areas: ['Model Town', 'Johar Town', 'Gulberg', 'DHA Phase 5', 'Samanabad'] },
    { city: 'Islamabad', province: 'Islamabad Capital Territory', postalCode: '44000', areas: ['Sector F-10', 'Sector G-11', 'Sector I-8', 'Bani Gala'] },
    { city: 'Rawalpindi', province: 'Punjab', postalCode: '46000', areas: ['Saddar', 'Satellite Town', 'Bahria Town', 'Chaklala Scheme III'] }
];

const FIRST_NAMES_MALE = ['Muhammad', 'Ahmed', 'Ali', 'Hamza', 'Bilal', 'Usman', 'Omar', 'Zain', 'Abdullah', 'Mustafa', 'Arsalan', 'Fahad', 'Saad', 'Talha', 'Haris'];
const FIRST_NAMES_FEMALE = ['Ayesha', 'Fatima', 'Sana', 'Amna', 'Hira', 'Khadija', 'Zainab', 'Anum', 'Mariam', 'Sidra', 'Iqra', 'Mahnoor', 'Laiba', 'Saba', 'Sara'];
const LAST_NAMES = ['Khan', 'Ahmed', 'Ali', 'Sheikh', 'Butt', 'Malik', 'Siddiqui', 'Abbasi', 'Qureshi', 'Shah', 'Mughal', 'Chaudhry'];

const BLOOD_GROUPS = ['A+', 'A-', 'B+', 'B-', 'AB+', 'AB-', 'O+', 'O-'];
const RELIGIONS = ['Islam', 'Islam', 'Islam', 'Islam', 'Christianity', 'Hinduism'];
const STREET_TYPES = ['Street', 'Lane', 'Road', 'Avenue'];

const ALL_POSSIBLE_SUBJECTS = [
    'English', 'Urdu', 'Mathematics', 'General Knowledge', 'General Science',
    'Social Studies', 'History_Geography', 'Computer Education', 'Physics',
    'Chemistry', 'Biology', 'Computer Science', 'Islamiat', 'Ethics', 'Pakistan Studies'
];

const CLASS_ACTIVE_SUBJECTS = {
    'Class 1': ['English', 'Urdu', 'Mathematics', 'General Knowledge'],
    'Class 2': ['English', 'Urdu', 'Mathematics', 'General Knowledge'],
    'Class 3': ['English', 'Urdu', 'Mathematics', 'General Knowledge'],
    'Class 4': ['English', 'Urdu', 'Mathematics', 'General Science', 'Social Studies'],
    'Class 5': ['English', 'Urdu', 'Mathematics', 'General Science', 'Social Studies'],
    'Class 6': ['English', 'Urdu', 'Mathematics', 'General Science', 'History_Geography', 'Computer Education'],
    'Class 7': ['English', 'Urdu', 'Mathematics', 'General Science', 'History_Geography', 'Computer Education'],
    'Class 8': ['English', 'Urdu', 'Mathematics', 'General Science', 'History_Geography', 'Computer Education'],
    'Class 9': ['English', 'Urdu', 'Mathematics', 'Physics', 'Chemistry', 'Biology_or_Computer'],
    'Class 10': ['English', 'Urdu', 'Mathematics', 'Physics', 'Chemistry', 'Biology_or_Computer', 'Pakistan Studies']
};

const usedUsernames = new Set();
const usedStudentIDs = new Set();
const usedAdmissionNumbers = new Set();

// ==========================================
// 2. HELPERS
// ==========================================

function getRandomElement(arr) { return arr[Math.floor(Math.random() * arr.length)]; }
function getRandomInt(min, max) { return Math.floor(Math.random() * (max - min + 1)) + min; }

function generatePakistaniMobile() {
    const networks = ['300', '301', '302', '312', '321', '333', '345'];
    return `+92${getRandomElement(networks)}${getRandomInt(1000000, 9999999)}`;
}

function generateUniqueId(prefix, length, usedSet) {
    let id;
    do {
        let num = '';
        for (let i = 0; i < length; i++) num += getRandomInt(0, 9);
        id = `${prefix}${num}`;
    } while (usedSet.has(id));
    usedSet.add(id);
    return id;
}

// ==========================================
// 3. REALISTIC MARKS GENERATOR (The Core Logic)
// ==========================================
function generateRealisticMarks(studentPerformanceType) {
    // 1. Excellent Student (A/A+)
    if (studentPerformanceType === 'excellent') {
        return getRandomInt(80, 100);
    }
    // 2. Average Student (B/C/D)
    else if (studentPerformanceType === 'average') {
        return getRandomInt(45, 79);
    }
    // 3. Struggling / Boundary Case
    else if (studentPerformanceType === 'struggling') {
        return getRandomInt(30, 48); // High chance of failing 1-2 subjects
    }
    // 4. Failing Student
    else if (studentPerformanceType === 'failing') {
        return getRandomInt(12, 32); // Consistently below passing mark (33)
    }
    // 5. Absent (No Show)
    else {
        return "A"; 
    }
}

// ==========================================
// 4. DATA GENERATION PIPELINE
// ==========================================

function generateStudentData() {
    const students = [];
    
    for (let classNum = 1; classNum <= 10; classNum++) {
        const className = `Class ${classNum}`;
        const sections = ['A', 'B', 'C'];
        
        for (let studentIndex = 1; studentIndex <= 30; studentIndex++) {
            const section = sections[Math.floor((studentIndex - 1) / 10)];
            const rollNum = studentIndex;

            // Student Performance Category Allocation (Real distribution)
            // 20% Excellent, 55% Average, 15% Struggling, 7% Failing, 3% Absent
            const rollChance = Math.random();
            let performanceType = 'average';
            if (rollChance < 0.20) performanceType = 'excellent';
            else if (rollChance < 0.75) performanceType = 'average';
            else if (rollChance < 0.90) performanceType = 'struggling';
            else if (rollChance < 0.97) performanceType = 'failing';
            else performanceType = 'absent';

            const baseAge = classNum + 5; 
            const age = getRandomInt(baseAge, baseAge + 1); 
            const dob = `${2026 - age}-${String(getRandomInt(1, 12)).padStart(2, '0')}-${String(getRandomInt(1, 28)).padStart(2, '0')}`;
            
            const gender = Math.random() > 0.5 ? 'Male' : 'Female';
            const firstName = gender === 'Male' ? getRandomElement(FIRST_NAMES_MALE) : getRandomElement(FIRST_NAMES_FEMALE);
            const lastName = getRandomElement(LAST_NAMES);

            const location = getRandomElement(CITIES_DATA);
            const studentId = generateUniqueId('STD', 6, usedStudentIDs);
            const admissionNum = generateUniqueId('ADM', 6, usedAdmissionNumbers);
            const religion = getRandomElement(RELIGIONS);

            // --------------------------------------------------
            // Unified Subjects & Marks Logic
            // --------------------------------------------------
            const activeSubjects = CLASS_ACTIVE_SUBJECTS[className];
            const subjectMarks = {};
            let totalObtained = 0;
            let activeSubjectsCount = 0;
            let failedAnySubject = false;
            let wasAbsentForAll = (performanceType === 'absent');

            ALL_POSSIBLE_SUBJECTS.forEach(subject => {
                if (activeSubjects.includes(subject)) {
                    let finalSubjectName = subject;
                    if (subject === 'Biology_or_Computer') {
                        finalSubjectName = Math.random() > 0.5 ? 'Biology' : 'Computer Science';
                    }

                    const marks = generateRealisticMarks(performanceType);
                    
                    if (marks === "A") {
                        subjectMarks[finalSubjectName] = "A"; // Absent marked as string
                    } else {
                        subjectMarks[finalSubjectName] = marks;
                        totalObtained += marks;
                        if (marks < 33) failedAnySubject = true;
                    }
                    activeSubjectsCount++;
                } else {
                    if (subject === 'Biology_or_Computer') {
                        subjectMarks['Biology'] = null;
                        subjectMarks['Computer Science'] = null;
                    } else {
                        subjectMarks[subject] = null;
                    }
                }
            });

            // Religious Subject
            const religiousSubjectName = (religion === 'Islam') ? 'Islamiat' : 'Ethics';
            const alternativeSubjectName = (religion === 'Islam') ? 'Ethics' : 'Islamiat';
            
            const religiousMarks = generateRealisticMarks(performanceType);
            if (religiousMarks === "A") {
                subjectMarks[religiousSubjectName] = "A";
            } else {
                subjectMarks[religiousSubjectName] = religiousMarks;
                totalObtained += religiousMarks;
                if (religiousMarks < 33) failedAnySubject = true;
            }
            activeSubjectsCount++;
            subjectMarks[alternativeSubjectName] = null;

            // Final Calculations
            const totalMaxMarks = activeSubjectsCount * 100;
            let percentage = 0;
            let status = 'Pass';
            let grade = 'F';

            if (!wasAbsentForAll) {
                percentage = Math.round((totalObtained / totalMaxMarks) * 100 * 100) / 100;
                
                // Real-world grading standard (Fails if percentage < 33% OR if they fail any core subject)
                if (percentage >= 80 && !failedAnySubject) grade = 'A+';
                else if (percentage >= 70 && !failedAnySubject) grade = 'A';
                else if (percentage >= 60 && !failedAnySubject) grade = 'B';
                else if (percentage >= 50 && !failedAnySubject) grade = 'C';
                else if (percentage >= 33 && !failedAnySubject) grade = 'D';
                else {
                    grade = 'F';
                    status = 'Fail';
                }
            } else {
                // Whole result absent
                status = 'Absent';
                grade = 'N/A';
                percentage = 0;
                totalObtained = 0;
            }

            const student = {
                studentId,
                admissionNumber: admissionNum,
                rollNumber: rollNum,
                firstName,
                lastName,
                gender,
                dateOfBirth: dob,
                age,
                class: className,
                section,
                bloodGroup: getRandomElement(BLOOD_GROUPS),
                religion,
                nationality: 'Pakistani',
                fatherName: `${getRandomElement(FIRST_NAMES_MALE)} ${lastName}`,
                parentMobileNumber: generatePakistaniMobile(),
                completeAddress: `House No. ${getRandomInt(1, 500)}, ${getRandomElement(STREET_TYPES)} ${getRandomInt(1, 15)}, ${getRandomElement(location.areas)}`,
                city: location.city,
                province: location.province,
                postalCode: location.postalCode,
                attendancePercentage: wasAbsentForAll ? `${getRandomInt(10, 45)}%` : `${getRandomInt(65, 100)}%`,
                
                // Subject Payload
                subjectWiseMarks: subjectMarks, 
                
                totalMarksObtained: totalObtained,
                maxPossibleMarks: totalMaxMarks,
                percentage,
                grade,
                passFailStatus: status,
                feeStatus: Math.random() > 0.2 ? 'Paid' : 'Unpaid',
                username: `${firstName.toLowerCase()}.${lastName.toLowerCase()}${getRandomInt(10, 99)}`,
                createdAt: new Date().toISOString()
            };

            students.push(student);
        }
    }

    // --------------------------------------------------
    // 5. INTRODUCING REAL-WORLD NOISE: DUPLICATES (2-3%)
    // --------------------------------------------------
    // We intentionally clone a few existing records to mimic system entry bugs
    const numDuplicates = 8; 
    for (let i = 0; i < numDuplicates; i++) {
        const randomIndex = getRandomInt(0, students.length - 1);
        const original = students[randomIndex];
        
        // Deep copy of student object to inject duplicate
        const duplicate = JSON.parse(JSON.stringify(original));
        
        // Let's make 4 of them exact duplicates (perfect duplicates)
        // and remaining 4 partial duplicates (same studentId/Roll but slightly different feeStatus or cell)
        if (i >= 4) {
            duplicate.parentMobileNumber = generatePakistaniMobile();
            duplicate.feeStatus = original.feeStatus === 'Paid' ? 'Unpaid' : 'Paid';
        }

        students.push(duplicate);
    }

    // Shuffle the dataset so duplicates are randomly scattered, not stuck at the end!
    return students.sort(() => Math.random() - 0.5);
}

// ==========================================
// 5. FORMATTING EXPORTERS & EXECUTION
// ==========================================

function jsonToCsv(jsonData) {
    const subjectHeaders = [
        'English', 'Urdu', 'Mathematics', 'General Knowledge', 'General Science', 
        'Social Studies', 'History_Geography', 'Computer Education', 'Physics', 
        'Chemistry', 'Biology', 'Computer Science', 'Islamiat', 'Ethics', 'Pakistan Studies'
    ];

    const baseHeaders = Object.keys(jsonData[0]).filter(k => k !== 'subjectWiseMarks');
    const finalHeaders = [...baseHeaders, ...subjectHeaders];
    
    const csvRows = [];
    csvRows.push(finalHeaders.join(','));

    for (const row of jsonData) {
        const values = finalHeaders.map(header => {
            let val = subjectHeaders.includes(header) ? row.subjectWiseMarks[header] : row[header];
            if (val === null || val === undefined) val = 'NULL'; 
            
            const stringVal = String(val);
            if (stringVal.includes(',') || stringVal.includes('"') || stringVal.includes('\n')) {
                return `"${stringVal.replace(/"/g, '""')}"`;
            }
            return stringVal;
        });
        csvRows.push(values.join(','));
    }
    return csvRows.join('\n');
}

function main() {
    console.log('🚀 Generating noisy, realistic Pakistani student dataset...');
    const data = generateStudentData();
    
    // Write JSON & CSV
    fs.writeFileSync(path.join(__dirname, 'students.json'), JSON.stringify(data, null, 2), 'utf8');
    fs.writeFileSync(path.join(__dirname, 'students.csv'), jsonToCsv(data), 'utf8');

    console.log(`✅ Success! Generated ${data.length} records.`);
    console.log('⚠️  Info: Includes ~25% overall fail/absent rates, and 8 intentional duplicate records for debugging.');
}

main();